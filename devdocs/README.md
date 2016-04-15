# Development documents for Amzi! Prolog + Logic Server
### amzi-apls-devdocs

*license.txt* — A copy of the MIT License, which covers the Amzi! Prolog + Logic Server projects.

*amzi_build.md* — How to build the core system and interfaces.  Does not include the Eclipse IDE plug-in.

*amzi_projects.md* — Overviews of the various sub-projects of the full Amzi! system.

*amzi_portability_extensibility.md* - Amzi! is designed to be an extremely portable and extensible implementation of Prolog.  This file contains information about how to port Amzi! to different environments, or how to extend Amzi! to interface with other tools or provide other services.

*Original WAM paper.pdf* — This is it, the paper that made Prolog practical.  David Warren’s original paper on how to build a virtual machine for Prolog, the Warren Abstract Machine (WAM).  At the core of the Amzi! system you’ll see the code that implements the design.

It is not necessary to understand the WAM to maintain Amzi!, but it is interesting. The best resource for learning the WAM is Ait-Kaci’s A Tutorial Reconstruction of Warren’s Abstract Machine.  It is out of print but can be free online for non-commerical use. Commercial users should compensate the author for use of this work that has been the basis of many Prolog vendor’s, including this one, understanding of the WAM.

*amzi_logic_manual.htm* — Amzi! started out as Alan Littleford’s Cogent Prolog.  It has been through numerous changes since then, but the core logic and architecture has remained pretty much the same.  This document, dating from release 3 of the software, documents a lot of the internals.  The names of files and variables have changed, but the basic ideas are the same.

