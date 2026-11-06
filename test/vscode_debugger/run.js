// Runs the debugger integration tests within VS Code's extension test host,
// against the bundled extension in `_dist` (see `make build-debug`).
//
// Usage: node test/vscode_debugger/run.js
//
// Environment:
//   VSCODE_TEST_EXECUTABLE  VS Code to use (downloaded otherwise): its
//                           executable, its command-line script (e.g.
//                           `/usr/bin/code`), or its installation directory
//   VSCODE_TEST_CACHE       Where to keep extensions the tested extension
//                           depends on (default: `.vscode-test/extensions`)
//   SUPERBOL_TEST_SUITES    Only run suites whose names contain one of these
//                           comma-separated words (e.g. `tasks,debug conf`)
//   SUPERBOL_TEST_JOBS      Number of VS Code instances that run the suites
//                           in parallel (default: 1); each instance runs a
//                           share of the selected suites, balanced by their
//                           weights in `main.ml`, and its output is shown
//                           once it is done
//   SUPERBOL_LSP            LSP server to use (default: the one built in
//                           `_build`, if any)

const cp = require("child_process");
const fs = require("fs");
const os = require("os");
const path = require("path");
const {
  downloadAndUnzipVSCode,
  resolveCliPathFromVSCodeExecutablePath,
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

/** Terminal profile of the test instance. */
const TEST_SHELL = { path: "bash", args: ["--noprofile", "--norc"] };

/** Settings of the test instance: VS Code must not wait for answers to
 * prompts, and the extension needs its LSP server to resolve tasks defined
 * in `tasks.json`. */
function writeUserSettings(userDataDir, root) {
  const lsp = process.env.SUPERBOL_LSP ||
        path.join(root, "_build/default/src/lsp/superbol-free/main.exe");
  const settings = {
    "debug.onTaskErrors": "abort",
    "task.problemMatchers.neverPrompt": true,
    "extensions.ignoreRecommendations": true,
    "workbench.startupEditor": "none",
    // Keeps VS Code's chat and agents quiet in the output.
    "chat.disableAIFeatures": true,
    // Programs run without debugging (Ctrl+F5) run in a terminal: use a
    // shell that ignores the user's startup files.
    "terminal.integrated.profiles.linux": { "superbol-test": TEST_SHELL },
    "terminal.integrated.profiles.osx": { "superbol-test": TEST_SHELL },
    "terminal.integrated.defaultProfile.linux": "superbol-test",
    "terminal.integrated.defaultProfile.osx": "superbol-test",
    "terminal.integrated.shellIntegration.enabled": false,
  };
  if (fs.existsSync(lsp)) {
    settings["superbol.lspPath"] = lsp;
  } else {
    console.warn(`No LSP server at ${lsp}: tasks from tasks.json will not resolve`);
  }
  const dir = path.join(userDataDir, "User");
  fs.mkdirSync(dir, { recursive: true });
  fs.writeFileSync(path.join(dir, "settings.json"), JSON.stringify(settings, null, 2));
}

/** Names of VS Code executables within an installation directory (as
 * unpacked by `@vscode/test-electron`, or installed by a package), and of
 * the command-line scripts in its `bin` sub-directory. */
const EXECUTABLES_IN_DIRECTORY = {
  win32: ["Code.exe", "Code - Insiders.exe", "VSCodium.exe"],
  darwin: ["Visual Studio Code.app/Contents/MacOS/Electron",
           "Contents/MacOS/Electron"],
}[process.platform] || ["code", "code-insiders", "codium"];
const CLI_SCRIPTS = process.platform === "win32"
      ? ["code.cmd", "code-insiders.cmd", "codium.cmd"]
      : ["code", "code-insiders", "codium"];

function isExecutableFile(file) {
  try {
    fs.accessSync(file, fs.constants.X_OK);
    return fs.statSync(file).isFile();
  } catch {
    return false;
  }
}

function executableIn(dir) {
  return EXECUTABLES_IN_DIRECTORY
    .map((name) => path.join(dir, name))
    .find(isExecutableFile);
}

/** Checks the value of `VSCODE_TEST_EXECUTABLE`: a VS Code executable, the
 * command-line script of a VS Code installation (like `/usr/bin/code`, which
 * usually links to `<installation>/bin/code`), or the directory of a VS Code
 * installation.  Returns the VS Code executable. */
function givenExecutable(given) {
  const fail = (why) => { throw new Error(`VSCODE_TEST_EXECUTABLE=${given}: ${why}`); };
  let file;
  try {
    file = fs.realpathSync(path.resolve(given));
  } catch {
    fail("no such file or directory");
  }
  if (fs.statSync(file).isDirectory()) {
    return executableIn(file) ||
      fail(`no VS Code executable (${EXECUTABLES_IN_DIRECTORY.join(", ")}) `
           + `in this directory`);
  }
  if (!isExecutableFile(file)) {
    fail("not an executable file");
  }
  const dir = path.dirname(file);
  if (path.basename(dir) === "bin" && CLI_SCRIPTS.includes(path.basename(file))) {
    // A command-line script: use the executable of its installation.
    return executableIn(path.dirname(dir)) ||
      fail(`${file} seems to be a command-line script, but there is no VS Code `
           + `executable in ${path.dirname(dir)}: give that executable instead`);
  }
  return file;
}

/** The command-line script of the installation of the given executable. */
function cliPath(executable) {
  const bin = path.join(path.dirname(executable), "bin");
  return CLI_SCRIPTS.map((name) => path.join(bin, name)).find(isExecutableFile)
    || resolveCliPathFromVSCodeExecutablePath(executable);
}

/** Value of `SUPERBOL_TEST_JOBS`. */
function jobCount() {
  const given = process.env.SUPERBOL_TEST_JOBS;
  if (given === undefined || given === "") {
    return 1;
  }
  const jobs = Number(given);
  if (!Number.isInteger(jobs) || jobs < 1) {
    throw new Error(`SUPERBOL_TEST_JOBS=${given}: expected a positive integer`);
  }
  return jobs;
}

/** Creates the isolated profile of a test instance: its user data, the
 * directory where the tests build their programs, and the workspace folder
 * for tests of build tasks and debug configurations. */
function makeProfile(root) {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "superbol-vscode-test-"));
  const profile = {
    dir,
    userDataDir: path.join(dir, "user-data"),
    workDir: path.join(dir, "work"),
    projectDir: path.join(dir, "project"),
  };
  fs.mkdirSync(profile.workDir);
  fs.mkdirSync(profile.projectDir);
  writeUserSettings(profile.userDataDir, root);
  return profile;
}

/** Runs the tests in a VS Code instance using `profile`, with the extra
 * environment `env`, and passes its output to `write(stream, chunk)`.
 * Resolves to the exit code of the instance (or the signal that killed
 * it). */
function runInstance({ executable, root, tests, extensionsDir, profile, env, write }) {
  const args = [
    profile.projectDir,
    "--no-sandbox",
    "--disable-workspace-trust",
    // Avoids errors of GPU processes, notably under xvfb.
    "--disable-gpu",
    `--user-data-dir=${profile.userDataDir}`,
    `--extensions-dir=${extensionsDir}`,
    `--extensionDevelopmentPath=${root}`,
    `--extensionTestsPath=${tests}`,
  ];
  return new Promise((resolve) => {
    const child = cp.spawn(executable, args, {
      env: {
        ...process.env,
        ...env,
        SUPERBOL_TEST_WORKDIR: profile.workDir,
        SUPERBOL_TEST_PROJECT: profile.projectDir,
      },
      stdio: ["ignore", "pipe", "pipe"],
    });
    child.stdout.on("data", (chunk) => write(process.stdout, chunk));
    child.stderr.on("data", (chunk) => write(process.stderr, chunk));
    let finished = false;
    const finish = (status) => {
      if (!finished) {
        finished = true;
        resolve(status);
      }
    };
    child.on("error", (err) => {
      write(process.stderr, `Failed to start ${executable}: ${err.message}\n`);
      finish("error");
    });
    // Waits for the streams to be closed, so that all output is written.
    child.on("close", (code, signal) => finish(code ?? signal));
  });
}

/** Runs `jobs` instances in parallel, each on a shard of the suites, and
 * shows the output of each one in a block once it is done.  Returns the
 * exit statuses of the instances. */
function runInstances(jobs, options, profiles) {
  if (jobs === 1) {
    return Promise.all([runInstance({
      ...options, profile: profiles[0], env: {},
      write: (stream, chunk) => stream.write(chunk),
    })]);
  }
  return Promise.all(profiles.map(async (profile, k) => {
    const output = [];
    const status = await runInstance({
      ...options, profile, env: { SUPERBOL_TEST_SHARD: `${k}/${jobs}` },
      write: (stream, chunk) => output.push([stream, chunk]),
    });
    process.stdout.write(`\n===== Job ${k + 1}/${jobs} (exit status: ${status}) =====\n`);
    for (const [stream, chunk] of output) {
      stream.write(chunk);
    }
    return status;
  }));
}

async function main() {
  const root = path.resolve(__dirname, "../..");
  const manifest = require(path.join(root, "package.json"));
  const tests = path.join(root, "_build/default/test/vscode_debugger/main.bc.js");
  const jobs = jobCount();
  const executable =
    process.env.VSCODE_TEST_EXECUTABLE
      ? givenExecutable(process.env.VSCODE_TEST_EXECUTABLE)
      : await downloadAndUnzipVSCode();
  const cli = cliPath(executable);
  const extensionsDir =
    process.env.VSCODE_TEST_CACHE || path.join(root, ".vscode-test", "extensions");

  // Isolate each test instance from the user's own profile, and from the
  // other instances.
  const profiles = [];
  try {
    for (let k = 0; k < jobs; k++) {
      profiles.push(makeProfile(root));
    }
    // VS Code refuses to activate the extension without its dependencies.
    // Install them before the instances start, as they share them.
    installDependencies(cli, extensionsDir, profiles[0].userDataDir,
                        manifest.extensionDependencies || []);
    const statuses =
      await runInstances(jobs, { executable, root, tests, extensionsDir }, profiles);
    const failed = statuses.filter((status) => status !== 0).length;
    if (jobs > 1) {
      console.log(`\n${jobs - failed} of ${jobs} jobs succeeded`);
    }
    return failed === 0;
  } finally {
    for (const profile of profiles) {
      fs.rmSync(profile.dir, { recursive: true, force: true });
    }
  }
}

main().then((ok) => {
  process.exitCode = ok ? 0 : 1;
}, (err) => {
  console.error("Failed to run tests:", err);
  process.exit(1);
});
