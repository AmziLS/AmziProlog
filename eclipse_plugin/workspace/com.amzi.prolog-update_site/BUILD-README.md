# Building the update site for the Eclipse plugin

1. Download and install [Eclipse PDE](https://www.eclipse.org/pde/)
    * An all in-one package can be obtained by chosing [Eclipse IDE for RCP and RAP Developers](https://www.eclipse.org/downloads/packages/)
2. Import all projects from `/eclipse_plugin/workspace` into Eclipse
3. In the project `com.amzi.prolog-update_site` right-click `site.xml` and choose: `Plug-in Tools|Build Site`
4. Wait for the background task to finish (progress bar in the bottom right) 
5. The result will be available in `../../../release/eclipse_plugin`
    * For easier synching with the update site, another copy of the same files will be available in    
    `../../../../eclipse_IDE_plugin_update_site`

## Automatically uploading the update site

If you already have cloned
https://github.com/AmziLS/eclipse_IDE_plugin_update_site into the same directory
as mentioned above, relevant files will be overwritten during the build (as
intended). Checking in the changes and synching with GitHub is enough to
complete the process. As users will update from
https://raw.github.com/AmziLS/eclipse_IDE_plugin_update_site/master/, they will
automatically obtain the latest changes.

In other words, the folder structure of the GitHub repositories should be like
this:

```
/AmziProlog
/eclipse_IDE_plugin_update_site
```

That is, eclipse_IDE_plugin_update_site is *not* a sub-folder of the AmziProlog
repository, but independent and at the same level. This is intentional, since
the update site is a binary distribution and should not be mixed with source
code.

## Troubleshooting

If the update_site cannot be built, make sure the feature name, path and version
under `Site Map|Managing the Site` in the site.xml-editor is still correct.
(The Site Manifest Editor can be opened by double clicking site.xml.) 

Incorrect features will appear faded/have a black and white icon. Unfortunately,
builds will fail silently when a configuration is erroneous. Common reasons are
slightly differing version numbers (such as a missing .0 at the end), renamed
features, or any other Amzi plugin projects that were renamed or had their
version number changed.

The simplest solution is to delete the faded feature, and add the correct /
updated one from the list of available features. Make sure to add it as child of
`amzi_eclipse_feature`.
The feature will have a name similar to this:
`com.amzi.prolog-ide_extension_feature`
