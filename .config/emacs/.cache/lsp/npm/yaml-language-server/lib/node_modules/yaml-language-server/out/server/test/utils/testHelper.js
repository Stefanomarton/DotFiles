"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.caretPosition = exports.setupLanguageService = exports.testFileSystem = exports.setupSchemaIDTextDocument = exports.setupTextDocument = exports.SCHEMA_ID = exports.TEST_URI = exports.toFsPath = void 0;
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Red Hat. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
const node_1 = require("vscode-languageserver/node");
const path = require("path");
const fs_1 = require("fs");
const yamlSettings_1 = require("../../src/yamlSettings");
const schemaRequestHandler_1 = require("../../src/languageservice/services/schemaRequestHandler");
const yamlServerInit_1 = require("../../src/yamlServerInit");
const vscode_languageserver_textdocument_1 = require("vscode-languageserver-textdocument");
const vscode_json_languageservice_1 = require("vscode-json-languageservice");
const yaml_documents_1 = require("../../src/languageservice/parser/yaml-documents");
const testsTypes_1 = require("./testsTypes");
function toFsPath(str) {
    if (typeof str !== 'string') {
        throw new TypeError(`Expected a string, got ${typeof str}`);
    }
    let pathName;
    pathName = path.resolve(str);
    pathName = pathName.replace(/\\/g, '/');
    // Windows drive letter must be prefixed with a slash
    if (pathName[0] !== '/') {
        pathName = `/${pathName}`;
    }
    return encodeURI(`file://${pathName}`).replace(/[?#]/g, encodeURIComponent);
}
exports.toFsPath = toFsPath;
exports.TEST_URI = 'file://~/Desktop/vscode-k8s/test.yaml';
exports.SCHEMA_ID = 'default_schema_id.yaml';
function setupTextDocument(content) {
    yaml_documents_1.yamlDocumentsCache.clear(); // clear cache
    return vscode_languageserver_textdocument_1.TextDocument.create(exports.TEST_URI, 'yaml', 0, content);
}
exports.setupTextDocument = setupTextDocument;
function setupSchemaIDTextDocument(content, customSchemaID) {
    yaml_documents_1.yamlDocumentsCache.clear(); // clear cache
    if (customSchemaID) {
        return vscode_languageserver_textdocument_1.TextDocument.create(customSchemaID, 'yaml', 0, content);
    }
    else {
        return vscode_languageserver_textdocument_1.TextDocument.create(exports.SCHEMA_ID, 'yaml', 0, content);
    }
}
exports.setupSchemaIDTextDocument = setupSchemaIDTextDocument;
exports.testFileSystem = { readFile: (fsPath) => fs_1.promises.readFile(fsPath).then((c) => c.toString()) };
function setupLanguageService(languageSettings) {
    const yamlSettings = new yamlSettings_1.SettingsState();
    process.argv.push('--node-ipc');
    const connection = (0, node_1.createConnection)();
    const schemaRequestHandlerWrapper = (connection, uri) => {
        return (0, schemaRequestHandler_1.schemaRequestHandler)(connection, uri, yamlSettings.workspaceFolders, yamlSettings.workspaceRoot, yamlSettings.useVSCodeContentRequest, exports.testFileSystem);
    };
    const schemaRequestService = schemaRequestHandlerWrapper.bind(this, connection);
    const telemetry = new testsTypes_1.TestTelemetry(connection);
    const serverInit = new yamlServerInit_1.YAMLServerInit(connection, yamlSettings, schemaRequestHandler_1.workspaceContext, schemaRequestService, telemetry);
    serverInit.connectionInitialized({
        processId: null,
        capabilities: vscode_json_languageservice_1.ClientCapabilities.LATEST,
        rootUri: null,
        workspaceFolders: null,
    });
    const languageService = serverInit.languageService;
    const validationHandler = serverInit.validationHandler;
    const languageHandler = serverInit.languageHandler;
    languageService.configure(languageSettings);
    return {
        languageService,
        validationHandler,
        languageHandler,
        yamlSettings,
        telemetry,
    };
}
exports.setupLanguageService = setupLanguageService;
/**
 * Derives the absolute `position` of the caret given `content` containing a virtual caret.
 * @param content The content of the document.
 * The caret is located in the content using `|` bookends.
 * For example, `content = 'ab|c|d'` places the caret over the `'c'`, at `position = 2`
 * @returns The absolute position of the caret.
 */
function caretPosition(content) {
    // console.log(`was: len: ${content.length}, content: "${content}", str: "${content.substring(position)}"`);
    // Find bookends `|.|` in content
    const position = content.search(/\|[^]\|/); // | -> any char including newline -> |
    if (position === -1)
        throw new Error('Error in test case: no caret found in content');
    // Elide bookends from content
    content = content.substring(0, position) + content.substring(position + 1, position + 2) + content.substring(position + 3);
    // console.log(`now: len: ${content.length}, content: "${content}", pos: ${position}, str: "${content.substring(position)}"`);
    return { position, content };
}
exports.caretPosition = caretPosition;
//# sourceMappingURL=testHelper.js.map