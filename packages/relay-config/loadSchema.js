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

const {JsonSchema} = require('@rushstack/node-core-library');
const path = require('path');

function loadSchema(): any {
  return JsonSchema.fromFile(
    path.join(__dirname, 'relay-compiler-config-schema.json'),
  );
}

module.exports = loadSchema;