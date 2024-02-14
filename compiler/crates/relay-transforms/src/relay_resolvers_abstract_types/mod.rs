/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashSet;
use std::sync::Arc;

use common::ArgumentName;
use common::Diagnostic;
use common::DiagnosticsResult;
use common::FeatureFlags;
use common::Location;
use common::NamedItem;
use common::WithLocation;
use docblock_shared::RELAY_RESOLVER_DIRECTIVE_NAME;
use docblock_shared::ROOT_FRAGMENT_FIELD;
use graphql_ir::FragmentDefinition;
use graphql_ir::InlineFragment;
use graphql_ir::LinkedField;
use graphql_ir::Program;
use graphql_ir::ScalarField;
use graphql_ir::Selection;
use graphql_ir::Transformed;
use graphql_ir::TransformedValue;
use graphql_ir::Transformer;
use schema::FieldID;
use schema::InterfaceID;
use schema::ObjectID;
use schema::SDLSchema;
use schema::Schema;
use schema::Type;

/// Transform selections on interface types.
///
/// First we locate fields which are interface types. Then we convert all of its
/// selections into inline fragments per concrete type with the same
/// selections.
pub fn relay_resolvers_abstract_types(
    program: &Program,
    feature_flags: &FeatureFlags,
) -> DiagnosticsResult<Program> {
    if !feature_flags
        .relay_resolver_enable_interface_output_type
        .is_fully_enabled()
    {
        return Ok(program.clone());
    }
    let mut transform = RelayResolverAbstractTypesTransform::new(program);
    let next_program = transform
        .transform_program(program)
        .replace_or_else(|| program.clone());

    if transform.errors.is_empty() {
        Ok(next_program)
    } else {
        Err(transform.errors)
    }
}

struct RelayResolverAbstractTypesTransform<'program> {
    program: &'program Program,
    errors: Vec<Diagnostic>,
}

impl<'program> RelayResolverAbstractTypesTransform<'program> {
    fn new(program: &'program Program) -> Self {
        Self {
            program,
            errors: Default::default(),
        }
    }
}

impl Transformer for RelayResolverAbstractTypesTransform<'_> {
    const NAME: &'static str = "RelayResolverAbstractTypesTransform";
    const VISIT_ARGUMENTS: bool = false;
    const VISIT_DIRECTIVES: bool = false;

    fn transform_inline_fragment(
        &mut self,
        inline_fragment: &InlineFragment,
    ) -> Transformed<Selection> {
        let selections = self.transform_selections(&inline_fragment.selections);
        // If our child selections had no changes, do not copy them until we have to replace them
        let selections_to_transform = match &selections {
            TransformedValue::Keep => &inline_fragment.selections,
            TransformedValue::Replace(replaced_selections) => replaced_selections,
        };
        let transformed_selections = transform_selections_given_parent_type(
            inline_fragment.type_condition,
            &self.program.schema,
            selections_to_transform,
        );
        match transformed_selections {
            TransformedValue::Keep => {
                if !selections.should_keep() {
                    Transformed::Replace(Selection::InlineFragment(Arc::new(InlineFragment {
                        selections: selections_to_transform.to_vec(),
                        ..inline_fragment.clone()
                    })))
                } else {
                    Transformed::Keep
                }
            }
            TransformedValue::Replace(transformed_selections) => {
                Transformed::Replace(Selection::InlineFragment(Arc::new(InlineFragment {
                    selections: transformed_selections,
                    ..inline_fragment.clone()
                })))
            }
        }
    }

    fn transform_fragment(
        &mut self,
        fragment: &FragmentDefinition,
    ) -> Transformed<FragmentDefinition> {
        let selections = self.transform_selections(&fragment.selections);
        let selections_to_transform = match &selections {
            TransformedValue::Keep => &fragment.selections,
            TransformedValue::Replace(replaced_selections) => replaced_selections,
        };
        let transformed_selections = transform_selections_given_parent_type(
            Some(fragment.type_condition),
            &self.program.schema,
            selections_to_transform,
        );
        match transformed_selections {
            TransformedValue::Keep => {
                if !selections.should_keep() {
                    Transformed::Replace(FragmentDefinition {
                        selections: selections_to_transform.to_vec(),
                        ..fragment.clone()
                    })
                } else {
                    Transformed::Keep
                }
            }
            TransformedValue::Replace(transformed_selections) => {
                Transformed::Replace(FragmentDefinition {
                    selections: transformed_selections,
                    ..fragment.clone()
                })
            }
        }
    }

    fn transform_linked_field(&mut self, field: &LinkedField) -> Transformed<Selection> {
        let selections = self.transform_selections(&field.selections);
        let selections_to_transform = match &selections {
            TransformedValue::Keep => &field.selections,
            TransformedValue::Replace(replaced_selections) => replaced_selections,
        };
        let schema = &self.program.schema;
        let field_type = schema.field(field.definition.item);
        let edge_to_type = field_type.type_.inner();
        let transformed_selections = transform_selections_given_parent_type(
            Some(edge_to_type),
            &self.program.schema,
            selections_to_transform,
        );
        match transformed_selections {
            TransformedValue::Keep => {
                if !selections.should_keep() {
                    Transformed::Replace(Selection::LinkedField(Arc::new(LinkedField {
                        selections: selections_to_transform.to_vec(),
                        ..field.clone()
                    })))
                } else {
                    Transformed::Keep
                }
            }
            TransformedValue::Replace(transformed_selections) => {
                Transformed::Replace(Selection::LinkedField(Arc::new(LinkedField {
                    selections: transformed_selections,
                    ..field.clone()
                })))
            }
        }
    }
}

// Transform selections on an interface type.
fn transform_selections_given_parent_type(
    entry_type: Option<Type>,
    schema: &Arc<SDLSchema>,
    selections: &Vec<Selection>,
) -> TransformedValue<Vec<Selection>> {
    if let Some(Type::Interface(interface_id)) = entry_type {
        let (selections_to_copy, mut selections_to_keep) =
            partition_selections_to_copy_and_keep(selections, interface_id, schema);
        if selections_to_copy.is_empty() {
            TransformedValue::Keep
        } else {
            selections_to_keep.append(&mut create_inline_fragment_selections_for_interface(
                schema,
                interface_id,
                &selections_to_copy,
            ));
            TransformedValue::Replace(selections_to_keep)
        }
    } else {
        // If no parent type is provided, skip transform
        TransformedValue::Keep
    }
}

// Partition selections on an interface type to copy to inline fragments
// on concrete types and to keep as is.
// Selections that should be copied are those that have different implementations
// across concrete types on the interface type (e.g. resolver field defined differently
// per concrete type.)
// Selections that should be kept are those that have the same implementations
// across concrete types (e.g. fields defined directly on the abstract type, or on server)
// or inline fragments that are on a concrete type.
fn partition_selections_to_copy_and_keep(
    selections: &[Selection],
    interface_id: InterfaceID,
    schema: &Arc<SDLSchema>,
) -> (Vec<Selection>, Vec<Selection>) {
    // True means selection should be copied
    selections
        .iter()
        .cloned()
        .partition(|selection| match selection {
            Selection::InlineFragment(inline_fragment) => inline_fragment.type_condition.is_none(),
            Selection::FragmentSpread(_) => false,
            Selection::Condition(_) => true,
            Selection::LinkedField(field) => concrete_types_have_different_implementations(
                interface_id,
                field.definition.item,
                schema,
            ),
            Selection::ScalarField(field) => concrete_types_have_different_implementations(
                interface_id,
                field.definition.item,
                schema,
            ),
        })
}

fn concrete_types_all_defined_on_server(
    schema: &Arc<SDLSchema>,
    concrete_types: &HashSet<ObjectID>,
) -> bool {
    !concrete_types.iter().any(|object_id| {
        let object = schema.object(*object_id);
        object.is_extension
    })
}

// Return true if concrete types have different implementations for the interface field
// with field_id.
fn concrete_types_have_different_implementations(
    interface_id: InterfaceID,
    field_id: FieldID,
    schema: &Arc<SDLSchema>,
) -> bool {
    let interface = schema.interface(interface_id);
    let interface_field = schema.field(field_id);
    // Interface field is a model resolver field defined with @rootFragment
    if let Some(resolver_directive) = interface_field
        .directives
        .iter()
        .find(|directive| directive.name == *RELAY_RESOLVER_DIRECTIVE_NAME)
    {
        if resolver_directive
            .arguments
            .named(ArgumentName(*ROOT_FRAGMENT_FIELD))
            .is_some()
        {
            return true;
        }
    }
    // TODO do we need to memoize this?
    let implementing_objects = interface.recursively_implementing_objects(Arc::as_ref(schema));
    if concrete_types_all_defined_on_server(schema, &implementing_objects) {
        return false;
    }
    // Any of the implementing objects' corresponding field is a resolver field
    let selection_name = interface_field.name.item;
    implementing_objects.iter().any(|object_id| {
        let concrete_field_id = schema
            .named_field(Type::Object(*object_id), selection_name)
            .expect("Expected field to be defined on concrete type");
        let concrete_field = schema.field(concrete_field_id);
        concrete_field
            .directives
            .iter()
            .any(|directive| directive.name.0 == RELAY_RESOLVER_DIRECTIVE_NAME.0)
    })
}

/**
 * Converts selections on an abstract type to selections on inline fragments on a concrete
 * type by changing the field IDs to those defined on the concrete types in the schema.
 */
fn convert_interface_selections_to_concrete_field_selections(
    concrete_type: Type,
    selections: &[Selection],
    schema: &Arc<SDLSchema>,
) -> Vec<Selection> {
    selections
        .iter()
        .map(|selection| match selection {
            Selection::LinkedField(node) => {
                let field_name = schema.field(node.definition.item).name.item;
                let concrete_field_id = schema
                    .named_field(concrete_type, field_name)
                    .expect("Expected field to be defined on concrete type");
                let definition = WithLocation::new(node.definition.location, concrete_field_id);
                Selection::LinkedField(Arc::new(LinkedField {
                    definition,
                    alias: node.alias,
                    arguments: node.arguments.clone(),
                    directives: node.directives.clone(),
                    selections: node.selections.clone(),
                }))
            }
            Selection::ScalarField(node) => {
                let field_name = schema.field(node.definition.item).name.item;
                let concrete_field_id = schema
                    .named_field(concrete_type, field_name)
                    .expect("Expected field to be defined on concrete type");
                let definition = WithLocation::new(node.definition.location, concrete_field_id);
                Selection::ScalarField(Arc::new(ScalarField {
                    definition,
                    alias: node.alias,
                    arguments: node.arguments.clone(),
                    directives: node.directives.clone(),
                }))
            }
            Selection::FragmentSpread(_) => selection.clone(),
            Selection::InlineFragment(_) => selection.clone(),
            Selection::Condition(_) => selection.clone(),
        })
        .collect()
}

fn create_inline_fragment_selections_for_interface(
    schema: &Arc<SDLSchema>,
    interface_id: InterfaceID,
    selections: &Vec<Selection>,
) -> Vec<Selection> {
    assert!(
        !selections.is_empty(),
        "Expected selections to be non-empty when copying to inline fragments on concrete type"
    );
    let interface = schema.interface(interface_id);
    let implementing_objects = interface.recursively_implementing_objects(Arc::as_ref(schema));
    let mut sorted_implementing_objects = implementing_objects.into_iter().collect::<Vec<_>>();
    sorted_implementing_objects.sort();
    sorted_implementing_objects
        .iter()
        .map(|object_id| {
            let concrete_type = Type::Object(*object_id);
            Selection::InlineFragment(Arc::new(InlineFragment {
                type_condition: Some(concrete_type),
                directives: vec![], // Directives not necessary here
                selections: convert_interface_selections_to_concrete_field_selections(
                    concrete_type,
                    selections,
                    schema,
                ),
                spread_location: Location::generated(),
            }))
        })
        .collect()
}
