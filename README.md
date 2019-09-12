My custom Emacs setup. It is separated into 5 different logical segments for ease of configuration.

**Initialization**

This is the very first part of the `init.el` file. Here the garbage collector is adjusted, the load-paths specified, the custom file defined, and the start of the Emacs server is issued.

**General utilities**

This is the general configuration section. I consider to be most important since it defines not only `ivy`, but also maps keys for `Dvorak`, adds functions for editing lines, sets up back-up files (or lack-there-of), and other general editing and non-editing functionalities. If Emacs starts with an error, it would be more difficult for me to use it without having these initial configs.

**Dev environment**

Here is where the development environment is created. For every language and mode, there is a clean setup. The more generic things, like magit, are pushed to the top of this segment.

**Window manipulation**

Here is the configuration of how Emacs handles frames and buffers. Things like window numbering, window sizes and manipulation, and moving and transposing windows is found here.

**Looks**

Here Emacs is made pretty. Theme loads, highlighting, fonts, cursor, bracket matching and coloring, file separation, and powerline setup are all here.

**Org setup**

This is the specific place for all org related things. If something is about org or any related features, it is here.
