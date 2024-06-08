module dissasembly.dissInstructions;

/+
    This file defines all the ways the user can impact the dissassembly, 
    and all the commands needed.

    Commands have to veriations: in-file, and CLI commands. The reason
    for this is that in the file we can determine the binary address
    the command is occuring, but not while executing a terminal command.

        Terminal                                      In-File
    ┌────────────────────────────────────────┬───────────────────────────┐
    |                                        |                           |
    | `insert-comment $1234 hello world`     |   `#! hello world`        |
    |                                        |                           |
    ├────────────────────────────────────────┼───────────────────────────┤
    |                                        |                           |
    | `edit-comments $1234`                  |    `; old   #! new`       |
    |                                        |                           |
    ├────────────────────────────────────────┼───────────────────────────┤
    |                                        |                           |
    | `rn old_label new_label`               |     `foo: #! rn bar`      |
    |                                        |                           |
    ├────────────────────────────────────────┼───────────────────────────┤
    |                                        |                           |
    |                                        | #! start split 'foo.asm'  |
    | 'split [pg N] $0000-FFFF newFile.asm'  | a:                        |          
    |                                        |    xor a, a               |
    |                                        | #! end split              |
    |                                        |                           |
    ├────────────────────────────────────────┼───────────────────────────┤
    |                                        |                           |
    |               `undo`                   |        `#!undo`           |
    |                                        |                           |
    ├────────────────────────────────────────┼───────────────────────────┤
+/


// 