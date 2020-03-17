
<!-- PROJECT LOGO -->
<br />
<p align="center">
  <a href="https://github.com/jxjo/Xoptfoil">
    <img src="images/logo.png" alt="Logo" width="800" >
  </a>
</p>

# Xoptfoil-JX

This is Xoptfoil-JX, a fork of Xoptfoil - the amazing airfoil optimizer by Daniel Prosser  

## About the project

The project was started to handle some off the difficulties when it comes to optimize an airfoil having more advanced requirements on the quality of the generated airfoil. 

Main changes and improvements made up to now based on the original Xoptfoil

* Aerodynamic target values for an operating point to ease the final tweaking of an optimization - or to reverse engineer an airfoil from an existing polar.
* Geometric target values to replace optimization based on geometric constraints
* Instead of Hicks Henne shape functions the geometric airfoil parameters thickness, camber and leading edge radius can be used to perform a lightweight and fast optimization
* Support for polar type 1 (fixed speed) and polar type 2 (fixed lift) optimization
* Generation of a complete polar set of the final airfoil to import into xflr5 or flow5 (part of the sub project 'The Strak Machine')
* Minor other modifications in Xoptfoil and visualizer


## Installation

OS X & Linux:

```sh
npm install my-crazy-module --save
```

Windows:

```sh
edit autoexec.bat
```

## Usage example

A few motivating and useful examples of how your product can be used. Spice this up with code blocks and potentially more screenshots.

_For more examples and usage, please refer to the [Wiki][wiki]._

## Development setup

Describe how to install all development dependencies and how to run an automated test-suite of some kind. Potentially do this for multiple platforms.

```sh
make install
npm test
```

## Documentation

## Meta

Your Name – [@YourTwitter](https://twitter.com/dbader_org) – YourEmail@example.com

Distributed under the XYZ license. See ``LICENSE`` for more information.

[https://github.com/yourname/github-link](https://github.com/dbader/)

## Contributing

1. Fork it (<https://github.com/yourname/yourproject/fork>)
2. Create your feature branch (`git checkout -b feature/fooBar`)
3. Commit your changes (`git commit -am 'Add some fooBar'`)
4. Push to the branch (`git push origin feature/fooBar`)
5. Create a new Pull Request

