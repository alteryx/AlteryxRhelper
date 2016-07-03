## {{ pluginName }} Plugin

<img src="{{ pluginName }}Icon.png" width=48 height=48></img> 

This repo contains the **{{ pluginName }}** tool in Alteryx. Shown below is a brief description of the contents. 

| File                                 | Description                                       |
|--------------------------------------|---------------------------------------------------| 
| {{ pluginName }}Config.xml           | Configuration for plugin (auto generated)         |
| {{ pluginName }}Gui.html             | Gui for plugin (auto generated)                   |
| {{ pluginName }}Icon.png             | Icon for plugin                                   |
| app.min.js                           | Script to interactively manipulate Gui.html       |
| app.css                              | Custom style sheet for Gui.html                   |
| Supporting_Macros/                   | Macro backend                                     |
| Supporting_Files/                    | Gui, Tests, Samples and Help                      |


#### Development

Use branches to work on features and bug fixes. Commit often. Send a PR to the upstream repo to merge your changes back in. Make sure to sync your clone with the upstream repo before sending a PR, so that merge conflicts are avoided.

The `source` files that will be modified directly include

1. Supporting_Macros/{{ pluginName }}.yxmc (backend)
2. Supporting_Macros/{{ pluginName }}1.R   (backend)
3. Supporting_Files/Gui/overrides.yaml (gui)
4. Supporting_Files/Gui/layout.html    (gui)
5. Supporting_Files/App/src/*          (gui)

Whenever you manipulate one of these source files, you can run the `createPluginFromMacro()` function shown below to update the plugin and then run `copyHtmlPlugin` to install it in Alteryx. Make sure that your working directory is the plugin directory and also remember to set `options(alterx.path = <path to alteryx directory>)`  before the install.

```r
library(AlteryxRhelper)
options(alteryx.path = <path to alteryx>)
createPluginFromMacro()
copyHtmlPlugin()
```
