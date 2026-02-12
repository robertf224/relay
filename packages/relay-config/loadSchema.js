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
  return JsonSchema.fromFile(getSchemaPath(), {
    customFormats: {
      uint8: {
        type: 'number',
        validate: data => data >= 0 && data <= 255,
      },
      uint: {
        type: 'number',
        validate: data => data >= 0 && data <= Number.MAX_SAFE_INTEGER,
      },
    },
  });
}

module.exports = loadSchema;
