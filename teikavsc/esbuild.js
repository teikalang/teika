const util = require("node:util");
const fs = require("node:fs/promises");
const esbuild = require("esbuild");
const exec = util.promisify(require("node:child_process").exec);

const production = process.argv.includes("--production");
const watch = process.argv.includes("--watch");


async function main() {
    const ctx = await esbuild.context({
        entryPoints: ["src/extension.ts"],
        bundle: true,
        format: "cjs",
        minify: production,
        sourcemap: !production,
        sourcesContent: false,
        platform: "node",
        outfile: "dist/extension.js",
        external: ["vscode"],
        logLevel: "silent",
        loader: {
            ".scm": "text",
            ".wasm": "file",
        },
        plugins: [
            esbuildProblemMatcherPlugin
        ]
    });

    await buildTreeSitterTeikaWasm();

    if (watch) {
        await ctx.watch();
    } else {
        await ctx.rebuild();
        await ctx.dispose();
    }
}

async function buildTreeSitterTeikaWasm() {
    const mod = "node_modules/tree-sitter-teika";
    await exec(`node_modules/.bin/tree-sitter build --wasm ${mod}`);
    await fs.rename("tree-sitter-teika.wasm", "src/tree-sitter-teika.wasm");
}

/**
 * @type {import('esbuild').Plugin}
 */
const esbuildProblemMatcherPlugin = {
    name: 'esbuild-problem-matcher',

    setup(build) {
        build.onStart(() => {
            console.log('[watch] build started');
        });
        build.onEnd(result => {
            result.errors.forEach(({ text, location }) => {
                console.error(`✘ [ERROR] ${text}`);
                console.error(`    ${location.file}:${location.line}:${location.column}:`);
            });
            console.log('[watch] build finished');
        });
    }
};

main().catch(e => {
    console.error(e);
    process.exit(1);
});

