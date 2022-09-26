<div align=center>

# DotFiles
  
[![AwesomeWM](https://img.shields.io/badge/AwesomeWM-git-blue.svg?logo=lua)](https://github.com/awesomeWM/awesome)

</div>

# Small Description

<img src="https://user-images.githubusercontent.com/80684231/192327751-5584bd54-a99d-495b-a545-e1d627d3bf3a.png" alt="img" align="right" width="500px">

This is my personal collection of configuration files.

The [setup section](#setup) will guide you through the installation process.

Here are some details about my setup:

+ **WM**: [AwesomeWM](https://github.com/awesomeWM/awesome/) 
+ **OS**: Arch Linux
+ **Shell**: [zsh](https://wiki.archlinux.org/index.php/Zsh)
+ **Terminal**: [kitty](https://github.com/kovidgoyal/kitty/) 
+ **Editor**: [Neovim](https://github.com/neovim/neovim/) 
+ **File Manager**: Nautilus
+ **Launcher**: [rofi](https://github.com/davatorium/rofi/)
+ **Browser**: Firefox
  

  
This repository contains my personal dotfiles for most of the programs I use on a daily basis. Primarily, this allows me to organize my rice and to easily set up        the computing experience I enjoy on any machine I come across. However, it also allows me to share my preferences with others. I think there are some interesting        things here worth discovering, and that's why I take the time to write (or at least try to write) sufficient comments and documentation.
My principles are to find a satisfactory balance between functionality and design while keeping an eye on resource consumption. I prefer keyboard-focused control over everything else and place a high value on visual consistency. I use Arch Linux as my daily driver, but there shouldn't be much here requiring this specific distribution, so using my configurations on other distros or, in the worst case, porting them shouldn't be too hard.

# Setup
Here are the instructions you should follow to replicate my AwesomeWM setup. 
I use stow to manage my dotfile i use [stow](https://www.gnu.org/software/stow/).

1. Clone my repo

         $ git clone https://github.com/Stefanomarton/DotFiles
 
2. Enter the directory and create the symlinks with stow

        $ cd DotFiles 
        $ stow .

## Dependencies
Fonts
+ [**Nerd Fonts**](https://www.nerdfonts.com/font-downloads)

Software
This are the essential packages for this configuration to worw 
          
        awesome neovim firefox flameshot rofi feh kitty zsh starship



  



