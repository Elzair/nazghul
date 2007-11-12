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


The environment is set. Now, to compile Nazghul correctly under Mingw:

- cd to the win32 directoryin Msys

- run the configure script (or do the same manually)

- CD to the nazghul src directory in MSys

- run make

- If everything goes ok, you'll have a nazghul.exe file ready shortly.
If you want to run it, just put the exe file, the DLLs in the
/mingw/lib directory, and all the files from the haxima-1.002 dir in
one directory, and run the exe from there.