/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 * @oncall relay
 */

'use strict';

const Ajv2020 = require('ajv/dist/2020');
const fs = require('fs');
const path = require('path');

function getSchemaPath(): string {
  if (process.env.NODE_ENV === 'test') {
    return path.resolve(
      process.cwd(),
      'compiler',
      'crates',
      'relay-compiler',
      'relay-compiler-config-schema.json',
    );
  }
  return path.join(__dirname, '..', 'relay-compiler-config-schema.json');
}

function loadSchema(): any {
  const schemaData = JSON.parse(
    fs.readFileSync(getSchemaPath(), 'utf8'),
    (key, value) => (key === 'format' && value === null ? undefined : value),
  );
  const ajv = new Ajv2020({allErrors: true});
  ajv.addFormat('uint8', {
    type: 'number',
    validate: (data: number) => data >= 0 && data <= 255,
  });
  ajv.addFormat('uint', {
    type: 'number',
    validate: (data: number) => data >= 0 && data <= Number.MAX_SAFE_INTEGER,
  });
  const validate = ajv.compile(schemaData);
  return {
    validateObject(config: any, filepath: string) {
      if (!validate(config)) {
        const errors = (validate.errors || [])
          .map(e => `  ${e.instancePath} ${e.message}`)
          .join('\n');
        throw new Error(`Config validation failed for ${filepath}:\n${errors}`);
      }
    },
  };
}

module.exports = loadSchema;
