![Kanto logo](https://github.com/eclipse-kanto/kanto/raw/master/logo/kanto.svg)

# Eclipse Kanto - Software Update

Software update on a device via script enables updates of any kind of software, predefined in your script. You can monitor the install and download process and resume it on start up.

This functionality is provided by the Eclipse Kanto as a software-update native application. It allows you to install on a device any kind of software you define in your software updatable module.

Use the following operations to manage your script-based software updatable module:
* Download operation – download software module and store it for feature use
* Install operation – download or update software module and then install it
* Operation progress – download and install operations support progress
* Artifact validation:
    * validate downloaded artifacts with provided hash
    * download operation will stop, if the artifact file size exceeds the expected size
* Resume on startup:
    * resume module execution on startup
    * resume partially downloaded files on startup
* Command line interface – CLI client providing access to all core configurations

## Community

* [GitHub Issues](https://github.com/eclipse-kanto/software-update/issues)
* [Mailing List](https://accounts.eclipse.org/mailing-list/kanto-dev)
