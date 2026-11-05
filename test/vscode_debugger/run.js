// Runs the debugger integration tests within VS Code's extension test host,
// against the bundled extension in `_dist` (see `make build-debug`).
//
// Usage: node test/vscode_debugger/run.js
//
// Environment:
//   VSCODE_TEST_EXECUTABLE  VS Code executable to use (downloaded otherwise)
//   VSCODE_TEST_CACHE       Where to keep extensions the tested extension
//                           depends on (default: `.vscode-test/extensions`)

const cp = require("child_process");
const fs = require("fs");
const os = require("os");
const path = require("path");
const {
  downloadAndUnzipVSCode,
  resolveCliPathFromVSCodeExecutablePath,
  runTests,
} = require("@vscode/test-electron");

const INSTALL_ATTEMPTS = 3;

/** Lists the extensions installed in `extensionsDir`, in lower case. */
function installedExtensions(cli, extensionsDir) {
  const out = cp.execFileSync(cli, [`--extensions-dir=${extensionsDir}`, "--list-extensions"],
                              { encoding: "utf8" });
  return new Set(out.split(/\r?\n/).map((e) => e.trim().toLowerCase()).filter((e) => e));
}

/** Installs the given extensions into the cache `extensionsDir`, unless they
 * are already there.  Only this step needs network access. */
function installDependencies(cli, extensionsDir, userDataDir, dependencies) {
  fs.mkdirSync(extensionsDir, { recursive: true });
  const installed = installedExtensions(cli, extensionsDir);
  for (const dependency of dependencies) {
    if (installed.has(dependency.toLowerCase())) {
      continue;
    }
    for (let attempt = 1; ; attempt++) {
      try {
        cp.execFileSync(cli, [`--user-data-dir=${userDataDir}`,
                              `--extensions-dir=${extensionsDir}`,
                              "--install-extension", dependency],
                        { stdio: "inherit" });
        break;
      } catch (err) {
        if (attempt >= INSTALL_ATTEMPTS) {
          throw err;
        }
        console.error(`Failed to install ${dependency} (attempt ${attempt}), retrying...`);
      }
    }
  }
}

async function main() {
  const root = path.resolve(__dirname, "../..");
  const manifest = require(path.join(root, "package.json"));
  const tests = path.join(root, "_build/default/test/vscode_debugger/main.bc.js");
  const vscodeExecutablePath =
    process.env.VSCODE_TEST_EXECUTABLE || (await downloadAndUnzipVSCode());
  const cli = resolveCliPathFromVSCodeExecutablePath(vscodeExecutablePath);
  const extensionsDir =
    process.env.VSCODE_TEST_CACHE || path.join(root, ".vscode-test", "extensions");

  // Isolate the test instance from the user's own profile.
  const profile = fs.mkdtempSync(path.join(os.tmpdir(), "superbol-vscode-test-"));
  const userDataDir = path.join(profile, "user-data");
  // Where the tests build their programs.
  const workDir = path.join(profile, "work");
  fs.mkdirSync(workDir);
  try {
    // VS Code refuses to activate the extension without its dependencies.
    installDependencies(cli, extensionsDir, userDataDir,
                        manifest.extensionDependencies || []);
    await runTests({
      vscodeExecutablePath,
      extensionDevelopmentPath: root,
      extensionTestsPath: tests,
      extensionTestsEnv: { SUPERBOL_TEST_WORKDIR: workDir },
      launchArgs: [
        "--disable-workspace-trust",
        `--user-data-dir=${userDataDir}`,
        `--extensions-dir=${extensionsDir}`,
      ],
    });
  } finally {
    fs.rmSync(profile, { recursive: true, force: true });
  }
}

main().catch((err) => {
  console.error("Failed to run tests:", err);
  process.exit(1);
});
