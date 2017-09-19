+++
Categories = ["assembler", "linux", "x86_64"]
Tags = ["linux", "x86_64", "assembly"]
date = "2017-07-13"
title = " Layout of a stack frame in x86_64 "
+++

Stack frame in x86_64
---------------

Just for a memo:

```
                         +--------------+
                    +    |              |
                    |    +--------------+
                    |    |              |
                    |    |     argN     |
                    |    |              |
                    |    +--------------+
                    |    |              |
                    |    |   arg(N-1)   |
                    |    +--------------+
                    |    |     .....    |
                    |    +--------------+
                    |    |              |
                    |    |     arg7     |  starts from 7'th argument for x86_64
                    |    |              |
                    |    +--------------+
                    |    |              |
                    |    |Return address|  %rbp + 8
Stack grows down    |    |              |
                    |    +--------------+
                    |    |              |
                    |    |     %rbp     |  Frame base pointer
                    |    |              |
                    |    +--------------+
                    |    |              |
                    |    |  local var1  |  %rbp - 8 <-- %rsp (at the beginning of function)
                    |    |              |
                    |    +--------------+
                    |    |              |
                    |    | local ^ar 2  |
                    v    +--------------+
                         |              |
                         |              |
                         +--------------+
```