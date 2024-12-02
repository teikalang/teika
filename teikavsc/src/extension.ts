import { workspace, ExtensionContext, commands, languages } from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

import { SemanticTokensProvider, LEGEND } from "./highlighting";

let client: LanguageClient;

const restartClient = async () => {
  const workspaceConfiguration = workspace.getConfiguration();
  const teikaServerPath = workspaceConfiguration.get<string>(
    "teika.server.path",
    ""
  );

  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  const serverOptions: ServerOptions = {
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
  const clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: "file", language: "teika" }],
    synchronize: {},
  };

  if (!client) {
    // Create the language client and start the client.
    client = new LanguageClient(
      "teika",
      "Teika Server",
      serverOptions,
      clientOptions
    );
  }

  if (client && client.isRunning()) {
    await client.restart();
  } else {
    // Start the client. This will also launch the server
    await client.start();
  }
};

export function activate(context: ExtensionContext) {
  context.subscriptions.push(
    commands.registerCommand("teika.server.restart", () => {
      // TODO: handle promise below
      restartClient();
    })
  );

  languages.registerDocumentSemanticTokensProvider({ language: "teika", scheme: "file" }, new SemanticTokensProvider(), LEGEND)
  restartClient();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

