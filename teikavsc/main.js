var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
import { workspace, commands } from "vscode";
import { LanguageClient, TransportKind, } from "vscode-languageclient/node";
let client;
const restartClient = () => __awaiter(void 0, void 0, void 0, function* () {
    const workspaceConfiguration = workspace.getConfiguration();
    const teikaServerPath = workspaceConfiguration.get("teika.server.path", "");
    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    const serverOptions = {
        run: {
            command: teikaServerPath,
            args: [],
            transport: TransportKind.stdio,
        },
        debug: {
            command: teikaServerPath,
            args: [],
            transport: TransportKind.stdio,
        },
    };
    // Options to control the language client
    const clientOptions = {
        // Register the server for plain text documents
        documentSelector: [{ scheme: "file", language: "teika" }],
        synchronize: {},
    };
    if (!client) {
        // Create the language client and start the client.
        client = new LanguageClient("teika", "Teika Server", serverOptions, clientOptions);
    }
    if (client && client.isRunning()) {
        yield client.restart();
    }
    else {
        // Start the client. This will also launch the server
        yield client.start();
    }
});
export function activate(context) {
    context.subscriptions.push(commands.registerCommand("teika.server.restart", () => {
        // TODO: handle promise below
        restartClient();
    }));
    restartClient();
}
export function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
