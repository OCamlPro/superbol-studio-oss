Building the LSP binary
=======================

You don't know :code:`opam`
---------------------------

The first thing you want to do is to install and initialize :code:`opam`:

.. code-block:: sh

  # install opam
  bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"

  # initialize opam
  opam init

Then you'll need a switch with the good version of :code:`ocaml` to install
SuperBOL with.

.. code-block:: sh

  opam switch create superbol-studio 4.14.2
  # you can change the `superbol-studio` by any name you want, just replace it
  # in the following occurences

You can now check the next section (:ref:`Know opam`) to install the SuperBOL
LSP on your system.

.. _Know opam:

You know :code:`opam`
---------------------

First you must clone the repo and install a dependency:

.. code-block:: sh

  git clone https://github.com/OCamlPro/superbol-studio-oss.git
  cd superbol-studio-oss
  opam install drom -y

Then you want to install SuperBOL (assuming the targeted install opam switch is
:code:`superbol-studio`):

.. code-block:: sh

  drom install --switch superbol-studio -y

Once the command is done running, you can check if everything went well with

.. code-block:: sh

  opam exec -- superbol-free --version
