<div align=center>

# DotFiles
  
[![AwesomeWM](https://img.shields.io/badge/AwesomeWM-git-blue.svg?logo=lua)](https://github.com/awesomeWM/awesome)

</div>

# Small Description

<img src="https://user-images.githubusercontent.com/80684231/192327751-5584bd54-a99d-495b-a545-e1d627d3bf3a.png" alt="img" align="right" width="500px">

This is my personal collection of configuration files.

The [setup section](#setup) will guide you through the installation process.

Each configuration is described in this file, you can find each configuration [below](#Configurations).

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

# Configuration
### Neovim

<div align=center>
  
![image](https://user-images.githubusercontent.com/80684231/192338331-b0bd3b3b-d508-419c-b350-98a1204bdf30.png)
  
</div>

The config is kind of modularized, it make simple to find and modify what you need. Each section is then require in the init.lua file. 
I use nvim for latex and markdown writing and some minor programming stuff in lua and python.

### Trydactil
In my opinion one of the best firefox extension, you can find more [here](https://github.com/tridactyl/tridactyl). 
It integrated VIM keybindings in firefox, with also a lot of other cool features.

![image](https://user-images.githubusercontent.com/80684231/192339889-6cb1f369-26da-4064-8d56-69285543b995.png)

I like the bookmark management in particular:
Any letter 'x' can be assigned to a bookmark, then you can type:
+ Open the bookmark binded to x in the curret tab
   
      go'x'

+ Open the bookmark binded to x in a new tab

      gn'x' 



