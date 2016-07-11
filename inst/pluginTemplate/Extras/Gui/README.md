## Instructions

This folder contains the following files


| File                             | Description                                         |
|----------------------------------|-----------------------------------------------------| 
| overrides.yaml                   | Configuration to override widget properties         |
| layout.html.sample               | Sample Layout for organizing widgets in Gui.html    |
| README.md                        | Instructions                                        |
| App/                             | Source files for app.min.js (optional)              |


#### Layout

The interface elements in the macro are laid out in a linear sequence in the UI by default. The `layout.html.sample` file reflects the default template in use. To customize the layout, you can rename `layout.html.sample` to `layout.html` and modify the layout of the interface elements. For example, if you want an element named `foo` to be put in a `div`, you can replace `{{ foo }}` with the following in `layout.html`

```html
<div id='div-for-foo'>
  {{ foo }}
</div>
```

This can be combined with javascript snippets in `../app.min.js` to hide and show this `div` based on the value of other interface elements. 

#### Overrides

By default, the properties for each interface element are directly extracted from the macro. However, there are times when you want to add properties that are only available in the HTML5 UI. You can use the file `overrides.yaml` to do this. Shown below is an example:

```yaml
number:
  default: 20
checkbox:
  showAsToggle: "true"
```

This override updates the default value of the `number` widget, while adding the property `showAsToggle="true"` to the `checkbox` widget to render it as a toggle switch.

#### App

For complex UIs, we use ES6 and build the source files into `app.min.js` and `app.min.css`. The `App` folder is used to develop this. It is optional and only needs to be used for sophisticated UIs.
