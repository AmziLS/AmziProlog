# Getting started with Amzi! Prolog

## Amzi! Prolog Binary Distribution

Binary Distributions of Amzi! Prolog are available under [Releases](https://github.com/AmziLS/AmziProlog/releases) and can be found, as usual on
GitHub, on the right side bar.

They contain all the necessary binaries to get started quickly without having
to build anything.

## Installation

Simply extract the release ZIP for your platform (see above) into any directory you wish.

Execute `Amzi Prolog Environment (Release).lnk` to get a command line with all
the necessary environment variables initialized correctly.

### Details

Make sure to adapt `bin/amzi_vars_win32.bat` and `bin/amzi_vars_win64.bat` to match
the installation paths of the installed dependencies (only necessary when using
MySQL, MSVC or Java).

For using IDEs such as MS Visual Studio or Eclipse it may be simpler to set
system wide environment variables once, using
`Configure Amzi Dirs (Release).bat`.

Your release environment will be *either* 32 or 64 bit, but not both at the same
time, i.e., you need to have two separate folders for Amzi! Prolog 32 and 64
bit, and set the environment variable accordingly: either using
`Amzi Prolog Environment (Release).lnk` to create a temporary environment or
`Configure Amzi Dirs (Release).bat` each time you switch between the global
environments.

### Install Eclipse-IDE plugin

For easier debugging and coding of Prolog programs, you can use the
Eclipse-plugin. Follow these [simple installation steps](https://github.com/AmziLS/AmziProlog/tree/master/eclipse_plugin#install).

## Running your first example

Open a command line window using `Amzi Prolog Environment (Release).lnk` as
mentioned above. Change into the directory `samples/prolog/tutorial_tests`
and execute `alis tutor_test_ui.pro`.

It will start a question and answering session about basic arithmetic.
See the [Tutorial Testing sample documentation](https://github.com/AmziLS/AmziProlog/tree/master/samples/prolog/tutorial_tests/doc.html
for further guidance.
