# Getting started with Amzi! Prolog

A lot of care was taken to get you started as quickly and as simply as possible.
The instructions have more detail to ensure you have the necessary information
for trouble shooting, but the basic steps are not complex.

## Amzi! Prolog Binary Distribution

Binary Distributions of Amzi! Prolog are available under [Releases](https://github.com/AmziLS/AmziProlog/releases) and can be found, as usual on
GitHub, on the right side bar.

They contain all the necessary binaries to get started quickly without having
to build anything.

## Installation

1. Download a current release ZIP of Amzi! Prolog for your platform (see above),
and unpack it anywhere you wish
2. Set the environment variable `AMZI_DIR` to the installation directory of
Amzi! Prolog
    * Hint: `AMZI_DIR` should point to the unpack directory / installation
	directory; it will contain the following folders: `/bin`, `/abin`, `/docs`,
	`/langbindings`, ...
    * You can do it **automatically** by executing
	`Configure Amzi Dirs (Release).bat`
3. Windows: install the `Visual C++ 2017 Redistributable` available in the list
of [latest supported Visual C++ downloads](https://support.microsoft.com/help/2977003/the-latest-supported-visual-c-downloads)
    * `vc_redist.x86.exe` for the 32 bit Amzi! Prolog distribution and
	`vc_redist.x64.exe` for the 64 bit release

Open `Amzi Prolog Environment (Release).lnk` to get a command line with all
the necessary environment variables initialized correctly, so you can execute
all the Amzi! Prolog tools, such as `alis` or `arun`, no matter which directory
you are in.

### Details

For using IDEs such as Visual Studio or Eclipse it may be simpler to set
system wide environment variables, using `Configure Amzi Dirs (Release).bat`.

Your release environment will be *either* 32 *or* 64 bit, but not both at the
same time. That is, you need to have two separate folders for Amzi! Prolog 32
and 64 bit, and set the environment variables accordingly:
  * execute `Amzi Prolog Environment (Release).lnk` to create a temporary 32/64
  bit command line environment.
  * optionally, execute `Configure Amzi Dirs (Release).bat` each time you want
  to globally set the 32/64 bit `AMZI_DIR` for use with IDEs or other tools not
  started from the command line environment.

### Install Eclipse-IDE plugin

For easier debugging and coding of Prolog programs, you can use the
Eclipse-plugin. Follow these [simple installation steps](eclipse_plugin#install).

## Running your first example

Open a command line window using `Amzi Prolog Environment (Release).lnk` as
mentioned above. Change into the directory `samples/prolog/tutorial_tests`
and execute `alis tutor_test_ui.pro`.

It will start a question and answering session about basic arithmetic.
See the [documentation for the Tutorial Testing sample](samples/prolog/tutorial_tests/doc.html)
for further guidance.

## Compiling and distributing a Prolog program

In the directory `samples/prolog/duckworld` you will find a `doc.html` that
explains how to compile, link, run, and distribute *Duckworld*, a simple
standalone Prolog program.

The linked Prolog program is called `dw.xpl`. To turn this into an executable
you have to copy `arun.exe` (of the correct bitness for your target operating
system!) to `dw.exe`.

Also copy `amzi.dll` to the same folder.

Now, Duckworld can be run on another Windows machine, if you:
  * copy the following three files:
    - `dw.xpl`
    - `amzi.dll`
    - `dw.exe`
  * and your target system has the VC++ runtime installed
    - see point 3 of [installation](#installation) above

## Documentation

Have a look at the [documentation](README.md#documentation) and especially
[Getting Started with Eclipse Amzi! Prolog](https://www.youtube.com/watch?v=EMxLnn2I9yo).
For an overview of useful concepts and the general architecture, refer to the
[readme](README.md).
