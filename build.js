require("esbuild")
    .build({
        logLevel: "info",
        entryPoints: [
            "_build/default/src/superbol-vscode-extension/superbol_vscode_extension.bc.js"
        ],
        bundle: true,
        external: ["vscode"],
        outdir: "out",
        platform: "node",
        target: "es6",
        sourcemap: true,
        minifyWhitespace: true,
        minifySyntax: true,
    })
    .catch(() => process.exit())