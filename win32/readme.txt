1) Obtaining the necessary Mingw packages

Go to www.mingw.org and get the following packages:

	MSYS-1.0.10.exe
	binutils-2.15.91-20040904-1.tar.gz
	gcc-core-3.4.2-20040916-1.tar.gz
	gcc-g++-3.4.2-20040916-1.tar.gz
	mingw-runtime-3.9.tar.gz
	mingw32-make-3.80.0-3.tar.gz
	w32api-3.6.tar.gz
	mingw-utils-0.3.tar.gz*

	* The mingw-utils package is optional.

2) Installing Mingw

- Install MSYS.
- When MSYS installation is done, considering {MSYS} as the MSYS
installation directory, untar all other mingw packages into
{MSYS}/mingw. Here's a way to do it using the MSYS shell only:
	- Run the MSYS shell.
	- Change to the /mingw directory (cd /mingw)
	- Untar all mingw packages there, using the general command:

		tar -zxvf package-name.tar.gz

	- Remove tarballs from the /mingw directory.

3) Installing the SDL libraries

- Get the SDL package from:

	http://www.smalltalk.com.br/offsite/SDL.zip

- Unzip it in the {MSYS}/mingw directory.

5) SDL_Mixer

- Get the Win32 Devel zip from 

http://www.libsdl.org/projects/SDL_mixer/

- Unzip it and move the contents to appropriate locations in the {MSYS}/mingw directory
(They were unfortunately packaged in a manner that wont work nicely with just directly unzipping.
Just stick the contents of each folder into the folder of the same name in mingw)

4) Installing libpng and zlib

- Get the libpng and zlib zip packages from the subareas off of

http://sourceforge.net/project/showfiles.php?group_id=204414

- Unzip them and move their contents to appropriate locations in the {MSYS}/mingw directory
(They were unfortunately packaged in a manner that wont work nicely with just directly unzipping.
Just stick the contents of each folder into the folder of the same name in mingw)

- With libpng, I had to further copy the contents of /include/libpng12 into the parent directory- 
I think the files in that directory were supposed to be symlinks, but they didnt work...

The environment is set. Now, to compile Nazghul correctly under Mingw:

- cd to the win32 directoryin Msys

- run the configure script (or do the same manually)

- CD to the nazghul src directory in MSys

- run make

- If everything goes ok, you'll have a nazghul.exe file ready shortly.
If you want to run it, just put the exe file, the DLLs in the
/mingw/lib directory, and all the files from the haxima-1.002 dir in
one directory, and run the exe from there.

If the win32/Makefile is out of date, you may get complaints about missing include files, 
or "undefined references". If you go the part of the Makefile where there is a huge
list of *.h, *.c and *.o files, and add whatever ones we forgot, then it should work.

You may also need to tweak the options in win32/config.h as well, especially in respect to
version number. Version number in the worlds/haxima/start-new-game can get out of date easily,
too...
