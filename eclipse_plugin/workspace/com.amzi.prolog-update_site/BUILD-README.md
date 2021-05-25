# Building the update site for the Eclipse plugin

1. Right-click `site.xml` and chose: `Plug-in Tools|Build Site`
2. Wait for the background task to finish (progress bar in the bottom right) 
3. The result will be available in `../../../../eclipse_IDE_plugin_update_site`

## Troubleshooting
If the update_site cannot be built, make sure the feature name, path and version
under `Site Map|Managing the Site` in the site.xml-editor is still correct.
(The Site Manifest Editor can be opened by double clicking site.xml.) 

Incorrect features will appear faded/have a black and white icon. Unfortunately,
builds will fail silently when a configuration is erroneous. Common reasons are
slightly differing version numbers (such as a missing .0 at the end), renamed
features, or any other Amzi plugin projects that were renamed or had their version
number changed.

The simplest solution is to delete the faded feature, and add the correct /
updated one from the list of available features. Make sure to add it as child of
`amzi_eclipse_feature`.
The feature will have a name similar to this:
`com.amzi.prolog-ide_extension_feature`
