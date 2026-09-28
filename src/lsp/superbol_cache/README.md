# SuperBOL Dependency-aware Cache

## Features

This library implements a shareable manifest-based caching system.
Its distinguishing features are:

- Entries registered in the cache may have dependent entries.  A cache
  miss on one of the (direct or indirect) dependencies of an entry
  entails a miss on the entry;

  An entry is looked up by a **shallow key**, computed from inputs
  known *before* the work (producer version, entry label, digest of
  the primary input, digest of the effective configuration). The
  shallow key addresses a **manifest**: the list of witnesses actually
  observed during the work, plus the digest of the result. A hit
  requires the shallow key to resolve *and* every recorded witness to
  still hold.

- Entries may denote:
  
  + a file (its name and its contents);
  
  + contents of directories reachable via copybook paths;
  
  + an environment variable and its status (unset, set with a value).

- File entries may be registered via either:

  + (i) a pair of a URI and the contents of the corresponding file; or

  + (ii) a path to a filename.

  On a file-system where a file exists that corresponds to a given
  file URI, a cache hit via the URI/contents pair implies a hit via
  the filename path, and vice versa.  Moreover, the library performs
  I/O only within its own cache directory.  Resolving a witness to a
  current value is delegated to a caller-supplied resolver, so that
  the LSP can answer from its open-document store and the CLI from the
  filesystem.

  Witnesses **do not need to match** across distinct platforms.

- Entries and cached values are stored inside a configurable directory
  of the file-system;

  + this cache directory may be used by distinct users;

- Several instances of the same library/binary may operate
  concurrently on the same directory;

- The cache pruning policy is to be selected among:

  + never removing any entry automatically;

  + remove entries if they have not been hit after a configurable
    amount of time;

  + keep the cache as a whole --- entries along with the values they
    hold --- under a configurable size, by removing those least
    recently hit.

  The latter two may be combined, in which case an entry is removed as
  soon as either calls for it.

- Cache entries can be monitored via a dedicated API, that lists
  entries along with its disk usage, its dependencies, the time/date
  it was last subject to a hit.

- The cache management library uses a versioned format.

  This format version appears in the directory path.  Instances of
  different versions may share a cache root without interfering. An
  unrecognised or older format is ignored, never misread, and is
  reclaimed by the pruner.

- The cache is shareable across multiple users, **not** across
  machines, builds, and OCaml versions.
