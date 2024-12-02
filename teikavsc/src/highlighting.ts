import path from "node:path"
import Parser from "web-tree-sitter"
import treeSitterWasmPath from "web-tree-sitter/tree-sitter.wasm"
import treeSitterTeikaWasmPath from "./tree-sitter-teika.wasm"
import highlightsScm from "tree-sitter-teika/queries/highlights.scm"

import vscode, { CancellationToken, DocumentSemanticTokensProvider, SemanticTokens, TextDocument } from "vscode"

const TOKEN_MAP: Record<string, { type: string, modifiers?: string[] }> = {
  "variable.parameter": { type: "parameter" }
};

const TOKEN_TYPES = ['function', 'variable', 'string', 'number', 'operator', 'parameter', 'type'];
const TOKEN_MODIFIERS: string[] = [];
export const LEGEND = new vscode.SemanticTokensLegend(TOKEN_TYPES, TOKEN_MODIFIERS);

const parserPromise = (async () => {
  await Parser.init({
    locateFile(_: string, scriptDirectory: string) {
      return path.join(scriptDirectory, treeSitterWasmPath)
    }
  });
  const parser = new Parser();
  const Teika = await Parser.Language.load(path.join(__dirname, treeSitterTeikaWasmPath));
  parser.setLanguage(Teika);
  const highlightQuery = Teika.query(highlightsScm);

  return { parser, highlightQuery }
})()

export class SemanticTokensProvider implements DocumentSemanticTokensProvider {
  async provideDocumentSemanticTokens(document: TextDocument, _token: CancellationToken): Promise<SemanticTokens> {
    const { parser, highlightQuery } = await parserPromise;
    const tree = parser.parse(document.getText());
    const highlights = highlightQuery.matches(tree.rootNode);

    const tokensBuilder = new vscode.SemanticTokensBuilder(LEGEND);
    highlights
      .flatMap(highlight => highlight.captures)
      .forEach(({ name, node }) => {
        let { type, modifiers } = this.getTokenClassification(name)
        if (TOKEN_TYPES.includes(type)) {
          var validModifiers = modifiers?.filter(modifier => TOKEN_MODIFIERS.includes(modifier)) ?? [];
          tokensBuilder.push(
            new vscode.Range(
              new vscode.Position(node.startPosition.row, node.startPosition.column),
              new vscode.Position(node.endPosition.row, node.endPosition.column),
            ),
            type,
            validModifiers,
          )
        }
      });

    return tokensBuilder.build()
  }

  private getTokenClassification(name: string) {
    if (TOKEN_MAP[name]) {
      return TOKEN_MAP[name]
    } else {
      const [type, ...modifiers] = name.split('.')
      return { type, modifiers }
    }
  }
}

