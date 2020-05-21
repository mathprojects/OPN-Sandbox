# OPN Sandbox
An odd perfect number (OPN), if any exists, is an odd number N such that sigma(N)=2*N where the sigma function is defined as the sum of all positive integer divisors of N. No OPN has been found nor have OPNs been proved to not exist. OPN Sandbox is a set of tools used for computing properties of numbers relevant to OPNs for the purpose of observing the data to help find ways of searching for OPNs or proving they do not exist. 

Contents
---
1. Introduction to the tools in OPN Sandbox
2. Supported platforms
3. Development
4. Quick start guide
5. More about GMP
6. Contributing
7. Authors
8. License
9. Recent news
10. Disclaimer

## 1. Introduction to the tools in OPN Sandbox 

OPN Sandbox is a visual application for Window and MacOS (and potentially Linux). It provides a means of entering input data in a grid or loading it from a file, processing the data, and showing the output in a grid. There is also a command line app, opnsprop, which can be used in a terminal window, and it is available for all three targeted platforms. 

## 2. Supported platforms

The software is built using tools that are capable of compiling for multiple platforms. Generally, the platforms targeted are:
* Windows (64-bit). OPN Sandbox is tested with Windows 10.
* macOS (64-bit, at least version 10.8) OPN Sandbox is tested with macOS 10.13.
* Linux (64-bit) OPN Sandbox is tested with openSUSE Leap 15.1 64-bit.

## 3. Development
The software is developed using Object Pascal making it very easy for anyone to at least make simple modifications to the code. The compilers used have visual interfaces making it very easy to compile the software.

### Development of OPN Sandbox (gui) 
The visual interface is development with Delphi 10.3 (Object Pascal) using the Firemonkey framework which allows the software to be compiled for multiple platforms. Currently Windows and Mac are the platforms supported since they can be compiled using the freely available Delphi Community Edition. Delphi is capable of compiling the application for Linux but it requires the commercial edition of Delphi, so at this time, only the command line application is provided for Linux.

### Development of the command line app (opnsprop)
The command line tool opnsprop is a tool that processes input from a file and produces an output file containing the same number properties as is produced by the visual app. The output can be analyzed in any other appropriate tool that can load comma-separated data (CSV file). This tool is developed as a console application and can be compiled either by Delphi or the Free Pascal Compiler (FPC). The FPC has an optional visual interface (Lazarus). FPC is a purely free and open-source product and supports all target platforms. At this time, the Windows and MacOS versions of opnsprop are compiled with Delphi and the Linux version is compiled with FPC.

### Library used for large precision integer and floating point computation
Large precision integer and floating point processing is done by [Gnu Multiprecision Arithmetic Library](http://www.gmplib.org). The binaries for this library are included in the github repository for OPN Sandbox. The original source of these binaries is the MingGw-W64 project for Windows 64-bit, Homebrew for macOS, and usually available from the package repository for Linux. The Delphi bindings for libgmp and wrapper originate from the [MPIR project](https://code.google.com/archive/p/gmp-wrapper-for-delphi).

## 4. Quick start guide
The source code is available, and it is very easy to get the development IDE's up and running. However, pre-compiled binaries are also provided to get up and running quickly. At this time, installers are not provided.

### System requirements
OPN Sandbox as a visual application is provided for Windows (64-bit) and macOS (64-bit). OPN Sandbox command line application is provided for those and also Linux (64-bit). They may not work on all variants of these systems.

### Windows
Get the latest version from the repository binaries directory for Win64 and unzip it to any location you want to store it. It does not have to be "installed", so you can put it anywhere you want. Then find OPNSandbox.exe and run it. If you already know that you have the GMP library installed system-wide on your computer and want to use that version (it must be the 64-bit version), then you can discard the gmp.dll file included in the download. Included in the download is the file opnsprop.exe which is the command line tool (optional).

### MacOS
Get the latest version from the repository binaries directory for MacOS. After you open it, if you don't see the OPN Sandbox file, you may need to look for the OPNSandbox "drive" on your desktop or in Finder. You need to copy or drag the OPN Sandbox app to your Application folder. Optionally, you can download the opnsprop.zip file, unzip and copy the command line tool opnsprop to anywhere you want to put it. To use opnsprop, libgmp must be installed on the computer. See the next section if you want to do that.

### Linux
Get the latest version from the repository binaries directory and unpack the file opnsprop wherever you want to put it. Before running it, you must have libgmp installed which you can do using your systems package manager.

### Processing the sample data
You can find some sample data in the repository sampledata directory (not included in the binaries downloads). For example, acquire the sample_input.csv file and put it somewhere on your computer. Run OPN Sandbox and select "Import...". Navigate to and select sample_input.csv. After it is loaded, click "Run". To process this file with the command line app, open a command prompt, powershell, or terminal window. On Windows, you need to change to the directory where you put the opnsprop file if it is not already in a directory in your system path. Type: opnsprop sample_input.csv sample_ouptut.csv and hit enter (you may have to specify the full path of the files). Assuming you have gmp.dll all set up, then the output file will be created and you can open it in Excel or a text editor. On Mac and Linux, unless you know how to put the command line app opnsprop in a directory that is in your system path, you will have to naviagate in a terminal window to the directory where opnsprop is located. The command is: ./openprop sample_input.csv sample_output.csv (you may have to specify full paths for input and output files). 

## 5. More about GMP

OPN Sandbox uses Gnu Multiple Precision Arithmetic Library (GMP) for processing large precision integers and floating point numbers. You can find more information about it at libgmp.org. The binaries for this library are provided in the repository and binaries for convenience because they can be hard to find, but ideally you would install it from another source. Below is a summary of how to install it for each platform. If you have a choice between 32- and 64-bit version, OPN Sandbox currently only works with the 64-bit version. If you have a choice between dynamic and static libraries, OPN Sandbox requires the dynamic gmp library.
* Windows - The version of gmp.dll provided here in the repository comes from the mingw-w64 project (http://mingw-w64.org). If you want to get a copy from them or see if they have a newer version available, you can go to their website and follow many links to get to the download for the individual file or just go directly to it at https://sourceforge.net/projects/mingw-w64/files/External%20binary%20packages%20%28Win64%20hosted%29/Binaries%20%2864-bit%29/ . 
* MacOS - On mac, the file is named something like libgmp.dylib. The version of libgmp.dylib provided here in the repository comes from the package repository for Homebrew (https://brew.sh). Using Homebrew is beyond the scope here but you would search for the libgmp package to install.
* Linux - The appropriate version of libgmp for Linux is so easy to get that it is not included in the binaries directory. Use the package manager in your OS to install libgmp.

## 6. Contributing

If you are interested in contributing to the development of this project, please consider contributing directly. See Authors for contact information.
 
## 7. Author(s)
* Matthew Thomas - *Initial work* [github: Math Projects](https://www.github.com/mathprojects), [Website: mattzart.com](http://www.mattzart.com), [Email: mattfwd@mywebsite](mailto:mattfwd@mattzart.com?subject=OPN%20Sandbox), [ORCID: [0000-0001-6511-3223]](http://orcid.org/0000-0001-6511-3223)
* Additional authors - Won't you contribute?

## 8. License

OPN Sandbox is licensed under the GNU Lesser General Public License V3 - see the [LICENSE.md](LICENSE.md) file for details.

## 9. Recent news

The original release - the standalone executable for Windows referred to in the paper "In Search of Odd Perfect Numbers" (DeDeo & Thomas) - is available under binaries/V1.0 for Windows 32- and 64-bit. It does not require GMP. It was briefly tested on Mac and it ran 100x slower than on Windows which is what necessitated finding a replacement for the Delphi Big Numbers library.

As of version 1.1, 32-bit systems will not necessarily be supported, but the software can be compiled for them, it is just time-consuming. The large number library was replaced with GMP. The command line application opnsprop was also introduced. Compiled binaries for the visual application and the command line app were completed for Windows and Mac. The command line app only is completed for Linux but the visual application should be able to be compiled for Linux by Delphi commercial edition (which I do not have due to lack of funding).

## 10. Disclaimer

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS, COPYRIGHT HOLDERS, OR DISTRIBUTORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

 
---
Copyright (c) 2020 Matthew Thomas

Updated 5/20/2020


 