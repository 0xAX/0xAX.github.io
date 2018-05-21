+++
Categories = ["bootloader", "grub"]
Tags = ["linux",  "grub"]
date = "2016-01-05"
title = "How does GNU GRUB work"
+++

This blog post is closely related with my interest in low-level stuff. As you already may know, I've started to be interested in such things like: How `N` works, where the `N` is something like - what does occur when we turn on the computer, pressing key on keyboard, how does an operating system load a program and many many more. I have found answers on some of my questions. You can look on the set if
blog [posts](http://0xax.github.io/tag/assembler.html) which are decribe some aspects of the [assembly](https://en.wikipedia.org/wiki/Assembly_language) programming or the [linux-insides](https://0xax.gitbooks.io/linux-insides/content/index.html) book which describes internals of the Linux kernel.

Yes. This answered on some of my questions, but not at all. Lately, besides the Linux kernel, I've also learned internals of the [GNU GRUB](https://en.wikipedia.org/wiki/GNU_GRUB). In the previous year I've got many thank you words from different people for the `linux-insides`. Seems that low-level stuff is interesting not only for me and I decided to write this blog post which will cover some parts of the `GNU GRUB` and we will see answer on the question which is in the title of this post - How GNU GRUB works. Hope, that it will be useful for somebody.

If you use Linux, you likely know about `GNU GRUB`. But just in case, wikipedia says that:

> GNU GRUB (short for GNU GRand Unified Bootloader) is a boot loader package from the GNU Project

So, the GNU GRUB is a bootloader. Main point of a bootloader is to load an operating system kernel and to transfer control to it. GNU GRUB has many features like support of different types of executable file formats, dynamic configuration, graphical menu interface, support for different types of file systems and etc.

So the point of the bootloader is clear - to load an operating system kernel. In this post we will see how the GNU GRUB loads the Linux kernel. But before this let's take a little look on architecture of the GNU GRUB. Even if you are newbie Linux user, you can guess that all `boot` related data is placed in the `/boot` directory. For me it looks like:

![boot-dir](https://s7.postimg.cc/g7w8jv6vd/ls-boot.png?dl=1)

This directory contains two [initrd](https://en.wikipedia.org/wiki/Initrd) images, the Linux kernel image and the `grub` directory which contains GNU GRUB related data:

![boot-grub-dir](https://s7.postimg.cc/hzp7etnob/ls-boot-grub2.png)

Directory with fonts, GNU GRUB configuration file, themes, locales, the `grubenv` file that contains GNU GRUB environment variables which are can be used in runtime and the `i386-pc` directory which contains GNU GRUB images and modules. Content of the `i386-pc` directory is the most interesting for us. Yes the `/boot/grub` directory contains many other interesting directories/files besides the `/boot/grub` directory, but this post will not cover topics like how to make GNU GRUB menu beautiful, readable and etc. If we will open the `/boot/grub/i386-pc` directory, we will find three types of files there:

* `*.lst` files - contain lists of available options depends on file. For example, the `/boot/grub/i386-pc/video.lst` contains list of available video modes or the `/boot/grub/i386-pc/fs.list` file contains information about supported file systems;
* `*.mod` files - 32-bit [ELF](https://en.wikipedia.org/wiki/Executable_and_Linkable_Format) files which provide additional functional for GNU GRUB. For example, the `/boot/grub/i386-pc/acpi.mod` adds support of the [Advanced Configuration and Power Interface](https://en.wikipedia.org/wiki/Advanced_Configuration_and_Power_Interface) which is used to perform various power-related functions or `/boot/grub/i386-pc/ext2.mod` provides support for [ext2](https://en.wikipedia.org/wiki/Ext2) file system;
* `*.img` files - only two files: `/boot/grub/i386-pc/boot.img` and `/boot/grub/i386-pc/core.img`. We need to look closer on this file.

The first `boot.img` file is the entry of the bootloader on a [PC BIOS](https://en.wikipedia.org/wiki/BIOS) system. The content of this file is written to the first sector of the disk or in the [Master boot record](https://en.wikipedia.org/wiki/Master_boot_record). The main point of the `boot.img` is to load first sector (512 bytes) of the `core.img` which will continue to do main job of the bootloader. As `boot.img` is in the master boot record, it must meet several conditions. First of all its size must be 512-bytes and the last two bytes must be `0xAA55`. We can see that `boot.img` is 512 bytes size file:

![boot.img](https://s7.postimg.cc/8f5krzgcr/boot-img-size.png)

and contains two magic bytes in the end:

```
~$ hexdump -s 510 /boot/grub/i386-pc/boot.img
00001fe aa55
```

Besides two magic bytes, the `Master boot record` must contain bootstrap code which will load second stage of the bootloader which can be much more than 512 bytes and [partition table](https://en.wikipedia.org/wiki/Partition_table) with four 16-bytes partition entries. Generall structure of the `MBR` must be like this:

```
0   +--------------------+
    |                    |
    |   Bootstrap code   |
446 |                    |
    |                    |
    |  Partition entry 1 |
    |  Partition entry 2 |
    |  Partition entry 3 |
    |  Partition entry 4 |
510 |        0x55        |
511 |        0xaa        |
512 +--------------------+
```

The second `core.img` file does the main job for us. It contains file system drivers, so it can load configuration from the `/boot/grub/grub.cfg` file and modules. The main point of the `core.img` is to transfer control to the last - second stage of the grub. At this moment, the GNU GRUB will have loaded modules, so it will know everything about operating system kernels which are needed to load. It draws menu, reacts on selection and etc.

Before we will start to dive into low-level source code of the GNU GRUB. We need to understand how all of this data occurs on a disk of computer. Besides bootloader functions, the GNU GRUB provides a rich set of utils:

![grub-utils](http://s22.postimg.org/z9plrxbld/grub_utils.png)

And one of this util may help us to install GNU GRUB on a computer. The name of this util is - `grub-install`. As we can read in the `grub-instal` manual page. The `grub-install` util:

  > grub-install - install GRUB on your drive

We just saw a little about GNU GRUB related files and now is time to see how the `grub-install` installs master boot record and this files. Let's look on the source code if the `grub-install`.

The grub-install util
----------------------

Implementation of the GNU GRUB utils is located in the `utils` directory. In our case, implementation of the `grub-install` utils is in the `grub-install.c`. If we will look on its `main` function, we will see that it starts from the call of the `grub_util_host_init` function which defined in the [grub-core/osdep/basic/init.c](http://git.savannah.gnu.org/cgit/grub.git/tree/grub-core/osdep/basic/init.c) source code file and produces standard stuff for a C programs, like the call of the `set_program_name`, setting locale and etc.

After the first inital initialization, we can see the call of the `arg_parse` function which as we may understand from the name - parses command line arguments of the `grub-install` util. We will not dive into details of implementation of the `argp_parse` function in ths post. I don't know how about you, but now, its interesting for me only low-level stuff in the GRUB. At the next step as we parsed command line arguments of the `grub-install` util, we start to check these arguments and do something depend on their values. First of all, we check the `-v` or `--verbose` flag which allows us to see verbose output of the `grub-instal` work. If this flag is set we set `debug=all` environment variable of GRUB with the call of the `grub_env_set` function:

```C
  if (verbosity > 1)
    grub_env_set ("debug", "all");
```

GRUB stores its environment variables in the hashtable which is represented by the following structure:

```C
struct grub_env_context
{
  struct grub_env_var *vars[HASHSZ];
  struct grub_env_context *prev;
};
```

The implementation of the `grub_env_set` function is simple. It just calculates index in the `grub_env_context` hashtable and stores a given variable in it. After this we can see the call of the:

```C
grub_util_load_config (&config);
```

function. This function just fills the `grub_util_config` structure from the GRUB configuration file (located in the `/etc/default/grub`). This structure consists from two fields. Both fields are depends on the following environment variables:

* `GRUB_ENABLE_CRYPTODISK` - allows to install GRUB on the encrypted disk.
* `GRUB_DISTRIBUTOR` - provides string which is associated with the distributor. For example, for me now it is:

```
$ cat /etc/default/grub | grep DIST
GRUB_DISTRIBUTOR="Arch"
```

After this we check the `GRUB_DISTRIBUTOR` value and if we've found it in the GRUB configuration file we save it in the `bootloader_id` variable. In other way the `bootloader_id` will contain `"grub"` string by default. At the next step we need to check current platform and exit in a failure case. The `grub-install` util does it with the call of the `get_default_platform` function. This function checks `gcc` directives and returns our platform:

```C
static const char *
get_default_platform (void)
{
#ifdef __powerpc__
   return "powerpc-ieee1275";
   ...
   ...
   ...
#elif defined (__amd64__) || defined (__x86_64__) || defined (__i386__)
   return grub_install_get_default_x86_platform ();
#else
   return NULL;
#endif
}
```

The `grub_install_get_default_x86_platform ()` function returns `x86_64-efi`, `i386-efi` or just `i386-pc` on `x86_64` platform. So, now we know target machine and now we need to get the path of directory where `grub-install` util will install its modules. In our case it will be `/lib/grub/i386-pc` and the `grub_install_source_directory` variable will contain this path. Besides the name of the target platform, we need to get information about this platform. The `grub-install` util will do it with the call of the `grub_install_get_target()` function. The main point of this function is to return item from the `platforms` array:

```C
static struct
{
  const char *cpu;
  const char *platform;
} platforms[GRUB_INSTALL_PLATFORM_MAX] =
  {
    [GRUB_INSTALL_PLATFORM_I386_PC] =          { "i386",    "pc"        },
    [GRUB_INSTALL_PLATFORM_I386_EFI] =         { "i386",    "efi"       },
    [GRUB_INSTALL_PLATFORM_I386_QEMU] =        { "i386",    "qemu"      },
	...
	...
	...
}
```

and print information about it:

```C
{
    char *platname = grub_install_get_platform_name (platform);
    fprintf (stderr, _("Installing for %s platform.\n"), platname);
    free (platname);
}
```

At the next step we need to select GRUB's disk module depends on the platform name. In our case it will be `biosdisk` module:

```C
switch (platform)
{
    case GRUB_INSTALL_PLATFORM_I386_PC:
      if (!disk_module)
	disk_module = xstrdup ("biosdisk");
      break;
	  ...
	  ...
}
```

The next step after we have selected disk module is initialization of all modules:

```C
  grub_init_all ();
  grub_gcry_init_all ();
  grub_hostfs_init ();
  grub_host_init ();
```

The source code of GRUB modules is located in different parts of GRUB source code, but each module contains definition of the `GRUB_MOD_INIT` and `GRUB_MOD_FINI` macros which make all initialization stuff. After all modules are initialized we are copying/installing to the `/boot/grub` directory all GRUB files (modules, locales, themes and etc.) to the source directory by the call of the:

```C
  grub_install_copy_files (grub_install_source_directory,
			   grubdir, platform);
```

function. After all of this manipulations, the `grub-install` util executes many different thing. It creates the `/boot/grub/envblk` file which is the GRUB environment block that is stores GRUB's environment variables. You can use the `grub-editevn --list` util to lust the GRUB environment variables. At the next step, the `grub-install` checks the given device, tries to understand type of files system on a given device and loads module for the certain file system type. It loads the module which provides functional for a disk reading. You can remember that it is the `biosdisk` for us. But the main point of the `grub-install` utils is to install [MBR](https://en.wikipedia.org/wiki/Master_boot_record), the `core.img` and the `kernel.img`. The most interesting part for us is the call of the:

```C
if (install_bootsector)
	grub_util_bios_setup (platdir, "boot.img", "core.img",
                          install_drive, force,
				          fs_probe, allow_floppy, add_rs_codes);
```

function. The `grub_util_bios_setup` function defined in the [util/setup.c](http://git.savannah.gnu.org/cgit/grub.git/tree/util/setup.c) source code file and its main point is to setup MBR. This function takes eight arguments:

* `platdir` - platform dependend directory wich contains GRUB modules, image and etc. (For example - `/lib/grub/i386-pc`);
* GRUB boot image;
* GRUB core image;
* `install_drive` - name of the device where to install GRUB;
* `force` - install or not if any problems are presented;
* `fs_probe` - allows GRUB to skip file system probes for the given device;
* `allow_floppy` - makes a drive bootable as floppy;
* `add_rs_codes` - shows apply or not [reed-solomon](https://en.wikipedia.org/wiki/Reed%E2%80%93Solomon_error_correction) codes during `core-img` embedding.

The `grub_util_bios_setup` function reads the `boot.img` and the `core.img` from the disk, sets the root device, copies [partition table](https://en.wikipedia.org/wiki/Partition_table) (will see more about it later), reads partition table, checks errors and writes the `core.img` and the `boot.img`:

```C
for (i = 0; i < nsec; i++)
	grub_disk_write (dest_dev->disk, sectors[i], 0,
		             GRUB_DISK_SECTOR_SIZE,
		             core_img + i * GRUB_DISK_SECTOR_SIZE);
...
...
...
if (grub_disk_write (dest_dev->disk, BOOT_SECTOR,
                     0, GRUB_DISK_SECTOR_SIZE, boot_img))
    grub_util_error ("%s", grub_errmsg);
```

That's all. Now we have installed master boot record and other boot-related GRUB parts on our machine.

Booting process
----------------

The booting process starts when BIOS reads first sector (first 512 bytes) from a disk and loads it into memory by `0x0000:0x7c000` address. The GNU GRUB MBR code is implemented in the [grub-core/boot/i386/pc/boot.S](http://git.savannah.gnu.org/cgit/grub.git/tree/grub-core/boot/i386/pc/boot.S) assembly source code file. As I already wrote above, the main point of the master boot record bootstrap code is to load second second sector from disk and control transfer to it. Besides this, bootstrap code does not do almost anything, because as you remember it is very small, only 512 bytes size. Let's look on the implementation of the bootstrap code.

In the beginning of the [grub-core/boot/i386/pc/boot.S](http://git.savannah.gnu.org/cgit/grub.git/tree/grub-core/boot/i386/pc/boot.S) source code file we can see definition of the global labels and the jump to the local label:

```assembly
.globl _start, start;
_start:
start:
	jmp	LOCAL(after_BPB)
	nop
```

The `LOCAL` macro defined in the [include/grub/symbol.h](http://git.savannah.gnu.org/cgit/grub.git/tree/include/grub/symbol.h) header file and expands to the concatenation of the `L_` and given symbol:

```C
#define LOCAL(sym)	L_ ## sym
```

in our case it will expand to the `L_after_BPB` label. This label represents the [BIOS parameter block](https://en.wikipedia.org/wiki/BIOS_parameter_block) which contains information about physycal layout of a disk. At the start of the `L_after_BPB` label we disable interrupts with the `cli` instruction to prevent erasing of the `dl` register which stores number of hard drive from which we have loaded. After this we test value of the `dl` register and set to `0x80` (first hard drive in the system) if buggy BIOS did not set it:

```assembly
	cli
    testb   $0x80, %dl
    jz      2f
2:
    movb    $0x80, %dl
```

At the next step we set data segment and stack segment registers to the known value - it is zero in our case, setup stack pointer to the top of the stack segment (`0x2000`) and enable interrupts again, because from this point we are safe now:

```assembly
	xorw	%ax, %ax
	movw	%ax, %ds
	movw	%ax, %ss

	movw	$GRUB_BOOT_MACHINE_STACK_SEG, %sp
	sti		/* we're safe again */
```

We just made enabled interrupts, so we can print `Welcome` message to the screen with the `MSG` macro:

```assembly
MSG(notification_string)

#define MSG(x)	movw $x, %si; call LOCAL(message)

notification_string:	.asciz "GRUB "

LOCAL(message):
	lodsb			/* loads character from %si to %al */
	cmpb	$0, %al /* check that we are at the end of string */
	jne	1b          /* display character if we are not at the end of string */
	ret

/* %si stores pointer to the notification_string */
/* %bx represents foreground color */
/* %ah number of BIOS service */
/* int $10 - http://www.ctyme.com/intr/rb-0106.htm */
1:  movw	$0x0001, %bx
	movb	$0xe, %ah
	int	$0x10
```

After we saw the `notification_string` in our screen, the `boot.S` starts to load first sector of the `core.img` file which is represented by the `diskboot.img` image. To read first sector of the `core.img` we will use the `0x42` function of the [0x13](http://www.ctyme.com/intr/rb-0708.htm) interrupt. First of all we need to check support of the [LBA](https://en.wikipedia.org/wiki/Logical_block_addressing) in the BIOS by the call of the [0x41](http://www.ctyme.com/intr/rb-0706.htm) fuction of the `0x13` interrupt:

```assembly
	movb	$0x41, %ah
	movw	$0x55aa, %bx
	int	$0x13
```

If the extended read or `LBA` is supported we start to read first 512 bytes from the `core.img`. To use extended read we must call the [0x42](http://www.ctyme.com/intr/rb-0708.htm) function of the `0x13` interrupt with the following set of arguments:

* `%ah` register must contain number of the function, `0x42` in our case;
* `%dl` register must contain number of the hard drive (starts from `0x80`);
* `%ds:%si` registers must point to the [disk address packet structure](http://www-pc.uni-regensburg.de/hardware/TECHDOK/ATA_EDD_11.PDF).

The disk address packet structure is a data structure which contains data that helps to convert logical block addressing information to physical parameters ([Cylinders, Heads, Sectors](https://en.wikipedia.org/wiki/Cylinder-head-sector)) of a disk. Before the call of the `0x13` interrupt, we need to fill disk address packet structure. In the our code it starts at the `disk_address_packet` label. General structure of the disk address packet structure is:

```
Offset
      +---------------------------------+
  0	  |  Packet size in bytes           |
  1   |  Reserved (must be 0)           |
  2   |  Number of blocks to transfer   |
  3   |  Reserved (must be 0)           |
  4   |  Address of transfer buffer     |
  8   |  Started absolute block number  |
      +---------------------------------+
```

Address of the disk address packet structure is located in the `%si` register:

```assembly
	movw	$disk_address_packet, %si
```

So, we can set reserved bytes to zero and packet size in our `disk_packet_packet` with the:

```assembly
movw	$0x0010, (%si)
```

packets size will be `16` bytes here. We will read one 512 bytes block:

```assembly
	xorw	%ax, %ax
	...
	incw	%ax
	movw	%ax, 2(%si)
```

In the end we need to set block number:

```assembly
	movl	LOCAL(kernel_sector), %ebx
	movl	%ebx, 8(%si)
	movl	LOCAL(kernel_sector_high), %ebx
	movl	%ebx, 12(%si)
```

and pointer to the buffer where we will read data from disk:

```assembly
	movw	$GRUB_BOOT_MACHINE_BUFFER_SEG, 6(%si)
```

and call the `0x13` interrupt:

```assembly
	movb	$0x42, %ah
	int	    $0x13
```

If all is good, the `GRUB_BOOT_MACHINE_BUFFER_SEG` will point to the beginning of the `diskboot.img` image in memory. In the of the [grub-core/boot/i386/pc/boot.S](http://git.savannah.gnu.org/cgit/grub.git/tree/grub-core/boot/i386/pc/boot.S) we relocate our buffer to the `GRUB_BOOT_MACHINE_KERNEL_ADDR` or address and jump into it:

```assembly
	jmp	*(LOCAL(kernel_address))
```

From this moment we have `diskboot.img` (which is first 512 bytes of the `core.img`) in the memory. As you may remember, the main point of the `diskboot.img` is to load rest of the `core.img` and jump into it. I will not describe this process here, it is pretty easy to understand if you understood previous description of how the `boot.S` loads `diskboot.img`. Both of these processes are prety similar. After the `diskboot.img` will load rest of the `core.img` it jumps to the GNU GRUB kernel code at [grub-core/boot/i386/pc/startup_raw.S](http://git.savannah.gnu.org/cgit/grub.git/tree/grub-core/boot/i386/pc/startup_raw.S) source code file. The main point of this code is to make preparation before the [C](https://en.wikipedia.org/wiki/C_%28programming_language%29) code. You can remember that we need to prepare [BSS](https://en.wikipedia.org/wiki/.bss) section for global uninitialized data to run `C` code and stack. Besides this we execute transition to [protected mode](https://en.wikipedia.org/wiki/Protected_mode). Let's look on this.

Before the transition to the protected mode, we set segment registers to the known value (zero in our case) and setup stack. After this we call the `real_to_prot` function which is implemented in the [grub-core/kern/i386/realmode.S](http://git.savannah.gnu.org/cgit/grub.git/tree/grub-core/kern/i386/realmode.S) assembly file. It starts from the loading of [Global Descriptor Table](http://wiki.osdev.org/Global_Descriptor_Table) with the `lgdt` instruction:

```assembly
lgdtl	gdtdesc
```

Where the `gdtdesc` contains description of four segments:

* Code segment;
* Data segment;
* Real-mode code segment;
* Real-mode data segment.

I will not describe what is it `GDT` and why do we need in it in this post. More about it you can read more about it in the second [part](https://0xax.gitbooks.io/linux-insides/content/Booting/linux-bootstrap-2.html) of the linux-insides book. After we set the `Global Descriptor Table`, we turn on protected mode by the setting `PE` bit in the `%cr0` [control register](https://en.wikipedia.org/wiki/Control_register):

```assembly
	movl	%cr0, %eax
	orl	$GRUB_MEMORY_CPU_CR0_PE_ON, %eax
	movl	%eax, %cr0
```

and jump to the protected mode. Next we clear segment registers, setup protected mode stack and load [interrupt descriptor table](https://en.wikipedia.org/wiki/Interrupt_descriptor_table) by the call of the `lidtl` instruction:

```assembly
lidt protidt
```

Interrupt descriptor table contains addresses of the interrupt handlers which are will be called when an interrupt occurs. After all of this manipulations we are in protected mode and may return to the [grub-core/boot/i386/pc/startup_raw.S](http://git.savannah.gnu.org/cgit/grub.git/tree/grub-core/boot/i386/pc/startup_raw.S) assembly file. In the end, we fill `%edx`, `%edi`, `%ecx` and `%eax` registers with the number of boot device, addresses of the `prot_to_real` and `real_to_prot` function which are helpers for transition between real/protected modes and address of the interrupt descriptor table. Now we can jump to the GNU GRUB kernel:

```assembly
movl	LOCAL(boot_dev), %edx
movl	$prot_to_real, %edi
movl	$real_to_prot, %ecx
movl	$LOCAL(realidt), %eax
jmp	*%esi
```

The GNU GRUB kernel for [x86](https://en.wikipedia.org/wiki/X86) entry is in the [grub-core/kern/i386/pc/startup.S](http://git.savannah.gnu.org/cgit/grub.git/tree/grub-core/kern/i386/pc/startup.S) assembly file. We are clearing space for the [BSS](https://en.wikipedia.org/wiki/.bss) section and call the first function which is written in `C` programming language:

```assembly
call EXT_C(grub_main)
```

For this moment, we've been through the low-level part of the GNU GRUB. Of course, it is not the end of the assembly. But for now we have loaded kernel of the GNU GRUB into memory and transfered control to it which is writen mostly in C programming language. Well, let's continue.

GNU GRUB kernel
----------------

The `grub_main` function defined in the [grub-core/kern/main.c](http://git.savannah.gnu.org/cgit/grub.git/tree/grub-core/kern/main.c) source code file and its main purpose is to initialize architecture-specific stuff, to load/parse configuration file and modules, to set some environment variables and to load `normal` mode. It starts from the call of the `grub_machine_init()` function which is defined in the [grub-core/kern/i386/pc/init.c](http://git.savannah.gnu.org/cgit/grub.git/tree/grub-core/kern/i386/pc/init.c) source code file. The `grub_machine_init` function initializes console by the call of the:

```C
grub_console_init ();
```

which just call of:

```C
grub_term_register_output ("console", &grub_console_term_output);
grub_term_register_input ("console", &grub_console_term_input);
```

functions. These functions takes two parameters: the first is name of a console and the second is pointer to a structure which contains pointer to the actions on a given console, like `putchar`, `cls` and etc. In the next time, when print-like function will be called, the GNU GRUB will go through a list of registered consoles and will call their print API. After this the `grub_machine_init()` function initializes memory regions and intializes [Time stamp counter](https://en.wikipedia.org/wiki/Time_Stamp_Counter).

After this we return to the `grub_main ()` function which calls the `grub_load_config()` function. As you can understand from the function's name, it loads configuration file. The next step is loading of GNU GRUB modules which are represented by the [ELF](grub_load_config ();) files in the `/boot/grub/arch`. For example:

![grub-mod](http://s3.postimg.org/bvfh7uatf/elf.png)

After the GNU GRUB kernel will load modules, it sets `root` and `prefix` environment variables which are represent root device and prefix of GNU GRUB directory (by default it is `/boot/grub`), parses configuration file, registers four core command: `ls`, `set`, `unset` and `insmod`. The last step of execution of the `grub_main ()` function is the `grub_load_normal_mode ()` function. This function defined in the same source code file as the `grub_main()` function and it tries to load and execute the `normal` module.

This module represents main module of GNU GRUB which starts to work after all of main low-level preparation. As we can read in the [documentation](http://www.gnu.org/software/grub/manual/grub.html#normal):

> In normal mode, commands, filesystem modules, and cryptography modules are automatically loaded, and the full GRUB script parser is available

So, the `normal` module/command is responsible for the user menu, loading of modules which are defined in the GNU GRUB configuration file with `insmod` command, handling of user input and transfering control to the real loader of an operating system kernel. Let's take a closer look on the `normal` mode.

Normal mode
------------

All code which is related to the `normal` mode is located in the [grub-core/normal](http://git.savannah.gnu.org/cgit/grub.git/tree/grub-core/normal) directory. As well as all the modules of GNU GRUB, the `normal.mod` has definition of the `GRUB_MOD_INIT` and `GRUB_MOD_FINI` macros which are responsible for a module initialization and finalization. The `normal.mod` sets a couple of environment variables like target processor, colors, pager and etc. Also it registers a set of command to clear screen, exit from normal mode and etc. In the end of the `grub_main()` function we could see the call of the:

```C
grub_command_execute ("normal", 0, 0);
```

function which executes already registred command in the `GRUB_MOD_INIT("normal")`. It looks:

```C
grub_register_command ("normal", grub_cmd_normal,
			           0, N_("Enter normal mode."));
```

So, after the last step of the `grub_main()` function, we will be in the `grub_cmd_normal()` function. This function enters to the `normal` mode. Practically it means that it reads configuration file of the GNU GRUB (`/boot/grub/grub.cfg`) to the `grub_menu_t` type which represents menu of the GNU GRUB and renders menu. We will skip many parts of this, like how menu renders and how it is represented in the source code of the GNU GRUB, because its not related directly with bootloading. The interesting part for us is that `grub_normal_execute()` function calls the `grub_show_menu()` function from the [grub-core/normal/menu.c](http://git.savannah.gnu.org/cgit/grub.git/tree/grub-core/normal/menu.c) source code file which in turn calls the `run_menu` function.

The `run_menu` function provides interruptible sleep until a menu item is selected and returns the selected menu item:

```C
boot_entry = run_menu (menu, nested, &auto_boot);
```

After the `run_menu()` function will return index of selected menu item which is represented by the `grub_menu_entry` structure, we need to execute this menu entry by the call of the `grub_menu_execute_entry` function. This function takes two parameters:

* Selected menu entry;
* `auto_boot` - shows that nothing was selected and timer is expired.

The `grub_menu_execute_entry()` function is big enough. It checks that an user selected submenu and renders it in this case, it checks that selected menu entry needs in authentication if we put something like `--users user1` to a menu entry and etc. But the greatest interest for us is the following code:

```C
grub_script_execute_new_scope (entry->sourcecode, entry->argc, entry->args);
...
...
...
if (grub_errno == GRUB_ERR_NONE && grub_loader_is_loaded ())
    grub_command_execute ("boot", 0, 0);
```

The first function takes the body of the selected menu entry, count of arguments and arguments of the GNU GRUB function and tries to execute it. For example if a body of your menu entry will be like this:

```
linux	/vmlinuz-linux root=UUID=4680b48e-595e-4d03-9115-2db79206e9f9 rw  quiet
echo	'Loading initial ramdisk ...'
initrd	 /initramfs-linux.img
```

The `grub_cmd_linux()`, `grub_cmd_initrd()` and the `grub_cmd_echo()` functions will be called. After the GNU GRUB will handle script we check an error and that loader is loaded. If both conditions will be good, we execute `boot` commands which will start to boot a kernel. Now we are stopped on the last step - loading of the Linux kernel.

Ok, finally lets load it
-------------------------

A loader will be loaded during `linux` command execution in the `grub_cmd_linux()` function. This function is defined in the [grub-core/loader/i386/linux.c](http://git.savannah.gnu.org/cgit/grub.git/tree/grub-core/loader/i386/linux.c) source code file. If you will look on the definition of the `linux` command in your `/boot/grub/grub.cfg` configuration file, you will see that this command takes path of the Linux kernel image as first argument. So, the `grub_cmd_linux()` function starts from the check of the number of command line arguments:

```C
if (argc == 0)
{
    grub_error (GRUB_ERR_BAD_ARGUMENT, N_("filename expected"));
	goto fail;
}
```

If we have no command line arguments, print error and go to `fail` label. After this we try to open and read the header of the Linux kernel image by the given path in the first command line argument. We are reading the Linux kernel image to the `struct linux_kernel_header lh` structure which defined in the [include/grub/i386/linux.h](http://git.savannah.gnu.org/cgit/grub.git/tree/include/grub/i386/linux.h) header file and contains fields which are mapped to the [Linux boot protocol v 2.10](https://www.kernel.org/doc/Documentation/x86/boot.txt). After we have read the header of the Linux kernel image, we make some checks for magic number (`0xaa55`), that `setup_sects` field is not greater than `64` and etc. As we finished with checks, we need to calculate size of the Linux kernel image and read it full. After this we need to fill/calculate fields which are marked in the Linux boot protocol as `write`. There fields are:

* `code32_start` - entry point of the Linux kernel in [protected mode](https://en.wikipedia.org/wiki/Protected_mode). It is not neccecary, but can be updated for realocation;
* `ramdisk_image` - the 32-bit linear address of the initial ramdisk or ramfs;
* `ramdisk_image_size` - the size of the initial ramdisk or ramfs;
* `heap_end_ptr` - the offset of the end of the setup stack/heap, minus `0x0200`;
* `loadflags` - bitmask which provides boot related flags.
* and etc.

So, we are setting the type of the bootloader which is the GNU GRUB (`0x72`) in our case, offset for the Linux kernel command line, `ramdisk_image` and `ramdisk_size` to zero (these fields will be filled in the `grub_cmd_initrd()`) and other fields:

```C
linux_params.type_of_loader = GRUB_LINUX_BOOT_LOADER_TYPE;
linux_params.cl_magic = GRUB_LINUX_CL_MAGIC;
linux_params.cl_offset = 0x1000;
linux_params.ramdisk_image = 0;
linux_params.ramdisk_size = 0;
linux_params.heap_end_ptr = GRUB_LINUX_HEAP_END_OFFSET;
linux_params.loadflags |= GRUB_LINUX_FLAG_CAN_USE_HEAP;
...
...
...
```

After we have load the Linux kernel image into memory and have filled Linux kernel header with constructed command line, we check errors and set callback function for the Linux kernel booting. In our case this function will be `grub_linux_boot()` function:

```C
if (grub_errno == GRUB_ERR_NONE)
{
    grub_loader_set (grub_linux_boot, grub_linux_unload, 0);
    loaded = 1;
}
```

where the `grub_loader_set()` function sets:

```C
grub_loader_set (grub_err_t (*boot) (void),
                 grub_err_t (*unload) (void),
		         int flags)
{
    ...
    grub_loader_boot_func = boot;
    ...
}
```

Now let's return to the `grub_menu_execute_entry()` function. We have stopped at the following code there:

```C
grub_script_execute_new_scope (entry->sourcecode, entry->argc, entry->args);

if (grub_errno == GRUB_ERR_NONE && grub_loader_is_loaded ())
    grub_command_execute ("boot", 0, 0);
```

The first line of code is executed and as we just saw, this function reads the body of a selected menu entry and executes commands from it. After the `grub_script_execute_new_scope` will be finished, at least `linux` and `initrd` commands of GNU GRUB will be executed. This means that the header of the Linux kernel and [initrd](https://en.wikipedia.org/wiki/Initrd) will be loaded into the memory and fields of the Linux kernel header are parsed and needed fields of it will be calculated and filled. If everything is ok, the `if` condition after the call of the `grub_script_execute_new_scope()` function will return true and the `boot` command will be execute.

The entry of the `boot` command is the `grub_cmd_boot()` function which defined in the [grub-core/commands/boot.c](http://git.savannah.gnu.org/cgit/grub.git/tree/grub-core/commands/boot.c) source code file. Besides a couple of check, the point of the `grub_cmd_boot()` function is to call `boot` callback function which is set by the loader:

```C
err = (grub_loader_boot_func) ();
```

In our case, this callback function was set in the `grub_loader_set()` function and this function is `grub_linux_boot()` function which defined in the [grub-core/loader/i386/linux.c](http://git.savannah.gnu.org/cgit/grub.git/tree/grub-core/loader/i386/linux.c) source code file. The `grub_linux_boot()` function sets video mode which is depends on values of `video` parameters from the Linux kernel header, sets the Linux kernel command line offset, fills register and start

```C
state.ebp = state.edi = state.ebx = 0;
state.esi = ctx.real_mode_target;
state.esp = ctx.real_mode_target;
state.eip = ctx.params->code32_start;
return grub_relocator32_boot (relocator, state, 0);
```

The relocator of the GNU GRUB is big piece of code which prepares all registers to the state which is good for the Linux kernel, prepares environment to the actual processor mode which depends on relocator type (may be in real, protected or long mode), calculates base physical address of the Linux kernel and jumps on it.

From this moment we are in the kernel!

That's all.

Conclusion
-----------

We saw how the GNU GRUB loads the Linux kernel in this post. Of course, it is not fully cover booting process of the Linux kernel and it also does not cover full aspects of the GNU GRUB. We have missed some things like how does GRUB manage filesystem related work, memory management related stuff, styles and appereance of menu and etc. This is not real to cover all of this topics and especially full source code of the such project like the GNU GRUB in one post. But I hope, you'll like it and you will research other boot related things with yourself.

**Questions/Suggestions**: Feel free about any questions or suggestions by pinging me at twitter [@0xAX](https://twitter.com/0xAX), adding an issue or just drop me an [email](kuleshovmail@gmail.com).

**Please note that English is not my first language and I am really sorry for any inconvenience. If you found any mistakes please send me PR to [0xax.github.com](https://github.com/0xAX/0xAX.github.io/tree/source) or just drop me an [email](kuleshovmail@gmail.com)**

Links
------

* [GNU GRUB](https://www.gnu.org/software/grub/)
* [GNU GRUB git web interface](http://git.savannah.gnu.org/cgit/grub.git/tree/)
* [assembly programming language](https://en.wikipedia.org/wiki/Assembly_language)
* [linux-insides](https://0xax.gitbooks.io/linux-insides/content/index.html)
* [initrd](https://en.wikipedia.org/wiki/Initrd)
* [ELF](https://en.wikipedia.org/wiki/Executable_and_Linkable_Format)
* [Advanced Configuration and Power Interface](https://en.wikipedia.org/wiki/Advanced_Configuration_and_Power_Interface)
* [Master boot record](https://en.wikipedia.org/wiki/Master_boot_record)
* [partition table](https://en.wikipedia.org/wiki/Partition_table)
* [BIOS parameter block](https://en.wikipedia.org/wiki/BIOS_parameter_block)
* [LBA](https://en.wikipedia.org/wiki/Logical_block_addressing)
* [Cylinders, Heads, Sectors](https://en.wikipedia.org/wiki/Cylinder-head-sector)
* [BSS](https://en.wikipedia.org/wiki/.bss)
* [protected mode](https://en.wikipedia.org/wiki/Protected_mode)
* [Global Descriptor Table](http://wiki.osdev.org/Global_Descriptor_Table)
* [interrupt descriptor table](https://en.wikipedia.org/wiki/Interrupt_descriptor_table)
* [Time stamp counter](https://en.wikipedia.org/wiki/Time_Stamp_Counter)
* [Linux kernel boot protocol](https://www.kernel.org/doc/Documentation/x86/boot.txt)
