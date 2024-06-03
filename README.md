# TiUtils

| :warning: WARNING                        |
|:-----------------------------------------|
| Heavy WIP, many features are incomplete  |

The most powerful method of deciphering and learning about a compiled binary for the TI-83 and TI-84. TiUtils includes an interactive disassembler, and binary identical reassembling. Due to the fact almost all TI signing keys are [public](https://wikiti.brandonw.net/index.php?title=83Plus:OS:Signing_keys), anything can be hacked, and therefore no technical reason would prevent us from simply recreating any binary.



## Basic Usage

```plaintext
TiUtils - A powerful, open source, Ti-83+ / Ti-84+ disassembler. (https://github.com/HeronErin/tiUtils)
Options:

TI help                                                     -Shows this menu
TI create [Project directory] [Path file to disassemble]    -Create a new project in a given directory
TI run [Project directory]                                  -Go back to an interactive editing session with a given file
TI info [Path file to look into]                            -Get a bunch of info for a given binary
```

## TI INFO

This command gives all information that can be gathered from a files properties and header values. The following are some example outputs:
```bash
    TI info bins/8xkFiles/whatangl.8xk
```
```plaintext
    Basic file overview:
            file type: App
            File Size: 32.85 KB
    In-File information (link file header information):
            Major Version: 0
            Minor Version: 16
            Flags: 1
            Object type: 88h
            Binary date (likely meaningless): 02/11/2002
            Name: "WhatAngl" ([57h, 68h, 61h, 74h, 41h, 6Eh, 67h, 6Ch])
            Device Type: TI83p (73h)
            Data Type: APPLICATION (24h)
            IntellHex length: 456.19 MB
    Binary header information (129 B) consiting of 12 fields:
            ProgramLength: 13.51 KB
            DeveloperKey: [01h, 04h]
            RevisionNumber: [01h]
            BuildNumber: [01h]
            Name: "WhatAngl" [57h, 68h, 61h, 74h, 41h, 6Eh, 67h, 6Ch]
            NumberOfPages: [01h]
            DisableSplashScreen: []
            MaxHardwareRevision: [01h]
            DateStamp: 2002-Feb-11 16:37:14+00:00  [09h, 04h, 09h, 9Eh, 4Ah, 3Ah]
            Signature: [98h, E3h, 95h, 9Ah, 62h, DAh, 39h, E9h, CAh, 53h, C1h, 8Bh, 4Eh, 09h, 50h, 26h, BFh, 32h, C8h, 1Fh, 6Ah, 2Bh, D0h, 59h, CCh, 1Ah, 4Eh, F0h, D9h, 2Ch, 9Eh, A9h, 6Dh, B3h, 0Ch, D2h, 9Fh, 06h, 7Ah, 4Ah, ACh, 4Bh, F9h, 93h, D7h, CAh, 46h, EEh, A8h, 79h, 4Bh, F2h, B8h, BCh, 02h, B2h, 79h, 68h, 96h, 9Ah, 3Ch, 75h, 1Ah, 19h]
            Padding: []
    Signature on final page: [77h, CFh, D8h, 83h, D3h, 58h, 7Ch, D3h, D3h, BFh, 8Ah, F0h, EEh, D8h, 23h, 9Bh, 54h, 07h, 01h, B6h, 51h, 0Fh, 8Ch, F3h, 82h, C0h, 6Bh, 0Ch, 69h, 9Dh, 27h, 6Eh, 76h, C4h, 6Bh, C0h, 52h, FFh, 42h, 4Ch, 9Fh, F4h, 4Fh, F8h, 1Fh, 87h, C0h, C5h, 35h, EBh, 42h, 0Ah, 80h, 35h, EDh, A1h, 11h, 99h, 5Fh, 8Ch, 1Dh, E0h, D1h, 5Bh]
```

```bash
    TI info bins/ti84plus_2.55.8xu
```

```plaintext
    Basic file overview:
            file type: OS
            File Size: 847.79 KB
    In-File information (link file header information):
            Major Version: 2
            Minor Version: 85
            Flags: 1
            Object type: 88h
            Binary date (likely meaningless): 10/19/2010
            Name: "basecode" ([62h, 61h, 73h, 65h, 63h, 6Fh, 64h, 65h])
            Device Type: TI83p (73h)
            Data Type: OS (23h)
            IntellHex length: 3.44 GB
    Binary header information (27 B) consiting of 7 fields:
            ProgramLength: 0 B
            DeveloperKey: [0Ah]
            RevisionNumber: [02h]
            BuildNumber: [37h]
            MaxHardwareRevision: [03h]
            NumberOfPages: [16h]
    Signature on final page: [39h, 68h, 36h, 58h, 57h, 9Eh, 7Ch, 91h, DEh, 05h, A3h, 7Dh, 4Fh, 80h, B4h, 98h, 24h, 70h, 12h, 66h, 04h, CBh, 43h, DEh, 80h, 35h, E1h, 8Fh, FAh, 2Fh, DAh, 04h, 8Bh, 36h, EEh, EFh, 4Dh, CAh, CEh, FEh, DCh, A8h, D3h, 28h, 76h, F7h, 00h, FFh, 52h, 6Ch, A9h, D3h, EAh, CBh, C7h, 84h, 12h, BFh, 36h, 3Ah, 02h, 6Ch, C1h, 38h]
```

## TI CREATE
TBD

## TI RUN
TBD