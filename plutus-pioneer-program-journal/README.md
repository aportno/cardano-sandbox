# Installations:
* Download Oracle VM Virtual Box Manager for Windows 10
* Create a VM with at least 8 gig ram and 100gb of storage using Ubuntu 64
* Start VM
* Select VM image for Ubuntu 20.04
* Follow normal installation process for Ubuntu on your system
* Once installation is complete, open a terminal
* Execute `sudo apt update` and `sudo apt upgrade -y` to update and upgrade software
* In the terminal, `sudo apt install git` and then `mkdir cardano` to create a cardano folder
* `cd` to `cardano`
* Enter `git clone https://github.com/input-output-hk/plutus` command
* Enter `git clone https://github.com/input-output-hk/plutus-pioneer-program` command
* `cd` back to your root directory
* We are going to install Nix, but first we will create a directory
* Enter `sudo mkdir /etc/nix` command
* Install VSCode or any other cod editor
* Create the file nix.conf in the Nix directory (path will be /etc/nix/nix.conf)
* Add the below to nix.conf and save:

````
substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/

trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
````
* In your terminal, `sudo apt install curl`
* Then install Nix by entering `curl -L https://nixos.org/nix/install | sh` command
* `cd` to `cardano/plutus` and `git check out <weekly tag>` described below in "gettting started"
* Next, run `nix build -f default.nix plutus.haskell.packages.plutus-core.components.library` in the terminal
* This will take some time to build, have patience! Once complete, you have completed the installation process


**Notes**:
1. You can adjust the size of your Orcal VM VirtualBox by going to `Devices` > `Insert guest additions CD image`
2. Hit run and then type in your password
3. Restart your VM
4. Log-on and then press `right ctrl` + `f` and then click `switch` on the prompt, this will open up `full screen`
5. To revert back to the minimized screen then once again `right ctrl` + `f` and then click `switch` on the prompt, this will open up `minimized screen`

---

## Getting started:

1. `cd` to `cardano/plutus-pioneer-program` file and enter `git pull` command
2. Goto `cardano/plutus-pioneer-program/code/week01` and enter `less cabal.project` command. **Note: weekly folder will change**
3. Copy the `tag` for the week i.e., week01 tag was `ea0ca4e9f9821a9dbfc52`
4. `cd` to `cardano/plutus` folder and enter `git checkout ea0ca4e9f9821a9dbfc52` command. **Note the branch tag will change weekly**
5. enter `git log` to ensure the `HEAD` is the same tagged you entered during `git checkout`
6. While in `cardano/plutus` enter `nix-shell` command
7. Once the `nix-shell` has been built, `cd` back to `cardano/plutus-pioneer-program/code/week01`
    * Note you will need to update for week02, week03, week_n etc
8. Build the code by entering `cabal build` command

## Starting a Playground server:

1. cd to `cardano/plutus` and `git pull` (make sure you are on the correct branch for the week) 
2. Once repo is updated, enter `nix-shell` command (while in cardano/plutus)
3. After nix-shell is built, `cd` to `cardano/plutus/plutus-playground-client`
4. Enter `plutus-playground-server` as a command. Wait until it respond with **Interpreter ready**
5. Open up another terminal and `cd` back to `cardano/plutus` and enter `nix-shell` as a command
6. `cd` to `cardano/plutus/plutus-playground-client`
7. Enter `npm run start` to start the playground client
8. Wait for the message `Compiled with warnings.` or another successful `compiled` alert
9. Open up localhost:8009 in your browser

#### Troubleshoot
* Failing to compile due to "Module Data.UUID has been defined multiple times". 
    * I deleted the Plutus repo and then re-cloned it, making sure to checkout the appropriate tag before running `nix-shell`
    * Alternatively, you can `rm -rf .spago` from the `plutus-playground-client` folder

## Tinkering in the Playground:
First we need to delete the default script in the Playground server and then compile the EnglishAuction.hs script

1. In a code editor, open:
   
`cardano/plutus-pioneer-program/code/week01/src/Week01/EnglishAuction.hs`

2. Copy the script (Ctrl + a) and navigate back to your browser tab with `localhost:8009`
3. Delete the default script in the Plutus playground editor and paste the EnglishAuction.hs script 
4. Hit **compile** in the top right corner of the Playground. It should say "complication successful" at the bottom of the screen
5. Scroll to the top right of the screen and click `simulator`

