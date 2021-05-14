# Amzi! Prolog

Source code for the entire Amzi! Prolog system.

Copyright (c) Amzi! inc. 2016, 2021
Licensed under the MIT Open Source License

# Contents

Amzi! Prolog's core is made of a Prolog Virtual Machine and a runtime library to access the VM, called Logic Server (see the engine folder for both).
The Logic Server's primary API is in C, but has also various foreign language bindings.

Several command line tools are available to "listen to", run, compile and link Prolog programs. Apart from a simple IDE (called wide/wide-IDE), there is also an Eclipse plugin that provides a more complete experience, including integrated debugging and compiling.

# Building

For building the whole system, see the make/ subdirectory. There are [detailed building instructions for Windows](https://github.com/AmziLS/AmziProlog/blob/master/Windows%20compilation%20instructions.md).

Individual components can be built using the make files in each subdirectory. It is also possible to load the .sln files into Visual Studio for debugging. Make sure that you have set up the environment variables correctly.

# Releases

[Releases](https://github.com/AmziLS/AmziProlog/releases) are available as usual on GitHub, on the right side bar. They contain the built runtime library, and command line tools, but also samples, language bindings, and ready to use Eclipse plugin. It's the easiest way to get started.
