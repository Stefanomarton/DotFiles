<div align=center>

![image](https://user-images.githubusercontent.com/80684231/192342745-7266665d-362b-42e4-acee-6ae3c7c6a633.png)
</div>

# Small Description

This is my personal collection of configuration files.

The [setup section](#setup) will guide you through the installation process.

Each configuration is described in this file, you can find each configuration [below](#Configurations).

<br> 
Here are some details about my setup:

<img src="![image](https://user-images.githubusercontent.com/80684231/193136221-4acd6235-7221-4978-9871-4774c3e7535e.png)" alt="img" align="right" width="500px">



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

In this config files you can find two different awesome setup:
+ Awesome 
+ Awesome-laptop

This is because I use different configuration file on laptop and desktop, I explain how I do it [below](https://github.com/Stefanomarton/DotFiles/edit/master/README.md#xinit).

# Setup

## SMABS (Stefano Marton Automatic Bootstrapping Script)
I'm experimenting with a small bootstrapping script, which deploy all my dotfiles and needed packages.
You can find it [here](https://github.com/Stefanomarton/SMABS)

## Stow 
Here are the instructions you should follow to replicate my AwesomeWM setup. 
I use stow to manage my dotfile i use [stow](https://www.gnu.org/software/stow/).

1. Clone my repo
    
       $ git clone https://github.com/Stefanomarton/DotFiles
 
2. Enter the directory and create the symlinks with stow

       $ cd DotFiles 
       $ stow .

## Manual cloning 

Just clone the repo and copy what you need where you need it

# Dependencies
Fonts
+ [**Nerd Fonts**](https://www.nerdfonts.com/font-downloads)

Software
This are the essential packages for this configuration to worw 
          
        awesome neovim firefox flameshot rofi feh kitty zsh starship

# Configurations
## Neovim

<div align=center>
  
![image](https://user-images.githubusercontent.com/80684231/192338331-b0bd3b3b-d508-419c-b350-98a1204bdf30.png)
  
</div>

The config is kind of modularized, it make simple to find and modify what you need. Each section is then require in the init.lua file. 
I use nvim for latex and markdown writing and some minor programming stuff in lua and python.

## Trydactil
In my opinion one of the best firefox extension, you can find more [here](https://github.com/tridactyl/tridactyl). 
It integrated VIM keybindings in firefox, with also a lot of other cool features.

![image](https://user-images.githubusercontent.com/80684231/192339889-6cb1f369-26da-4064-8d56-69285543b995.png)

I like the bookmark management in particular.

Any letter 'x' can be assigned to a bookmark, then you can type:
+ Open the bookmark binded to x in the curret tab
   
      go'x'

+ Open the bookmark binded to x in a new tab

      gn'x' 


## Starship

Starship is a cross shell prompt, highly configurabhe, with a lot of module available, without all the bloat that comes with p10k for oh-my-zsh, for example.

I keep it very simple with a kind of lambda theme, vi-mode indicator, directory path and git module. 

## ZSH

Simple config: I use some plugins, with zpico as a plugin manager, as I don´t need much other than this.
Several aliases are configured to make faster interaction with the terminal. 

I have integrated fzf as much as I can, I find it efficient, especially when searching throug history or files.

## Xinit

As I have different needs in my two different devices I use a simple case function in the .xinitrc file:


    case "$HOSTNAME" in 

    desktop)
      exec awesome
      ;;

    laptop)
      exec awesome ~/.config/awesome-laptop/rc.lua
      ;;

    esac

Changing the $HOSTNAME I can set what I need.
If you don't know how to change hostname after installation, you can read [here](https://wiki.archlinux.org/title/Network_configuration#Set_the_hostname)
