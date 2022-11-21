import { SettingsState } from '../../src/yamlSettings';
import { FileSystem } from '../../src/languageservice/services/schemaRequestHandler';
import { LanguageService, LanguageSettings } from '../../src';
import { ValidationHandler } from '../../src/languageserver/handlers/validationHandlers';
import { LanguageHandlers } from '../../src/languageserver/handlers/languageHandlers';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { TestTelemetry } from './testsTypes';
export declare function toFsPath(str: unknown): string;
export declare const TEST_URI = "file://~/Desktop/vscode-k8s/test.yaml";
export declare const SCHEMA_ID = "default_schema_id.yaml";
export declare function setupTextDocument(content: string): TextDocument;
export declare function setupSchemaIDTextDocument(content: string, customSchemaID?: string): TextDocument;
export declare const testFileSystem: FileSystem;
export interface TestLanguageServerSetup {
    languageService: LanguageService;
    validationHandler: ValidationHandler;
    languageHandler: LanguageHandlers;
    yamlSettings: SettingsState;
    telemetry: TestTelemetry;
}
export declare function setupLanguageService(languageSettings: LanguageSettings): TestLanguageServerSetup;
/**
 * Derives the absolute `position` of the caret given `content` containing a virtual caret.
 * @param content The content of the document.
 * The caret is located in the content using `|` bookends.
 * For example, `content = 'ab|c|d'` places the caret over the `'c'`, at `position = 2`
 * @returns The absolute position of the caret.
 */
export declare function caretPosition(content: string): {
    position: number;
    content: string;
};
