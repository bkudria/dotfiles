declare var settings: any;

export const applyDarkTheme = () => {
  // gruvbox dark
  api.Hints.style(`
    font-size: 1ex;
    color: #3c3836;
    font-family: "Menlo";
    background: #fbf1c7;
    `);

  settings.theme = `
    .sk_theme {
        font-family: "Iosevka Nerd Font", system-ui, sans-serif;
        font-size: 14px;
        background: #282828;
        color: #ebdbb2;
    }
    .sk_theme tbody {color: #b8bb26;}
    .sk_theme input {color: #d4c4a1;}
    .sk_theme .url {color: #98971a;}
    .sk_theme .annotation {color: #b16286;}
    .sk_theme .omnibar_highlight {color: #ebdbb2;}
    .sk_theme #sk_omnibarSearchResult ul li:nth-child(odd) {background: #282828;}
    .sk_theme #sk_omnibarSearchResult ul li.focused {background: #3c3836;}
    #sk_status, #sk_find {font-size: 20px;}
    div.hint-scrollable {background: #fbf1c7!important;}
  `;
};

export const applyLightTheme = () => {
  // gruvbox light
  api.Hints.style(`
            font-size: 8pt;
            color: #fbf1c7;
            font-family: "Menlo";
            background: #3c3836;
  `);

  settings.theme = `
    .sk_theme {
        font-family: "Iosevka Nerd Font", system-ui, sans-serif;
        font-size: 14px;
        background: #fbf1c7;
        color: #3c3836;
    }
    .sk_theme tbody {color: #79740e;}
    .sk_theme input {color: #d4c4a1;}
    .sk_theme .url {color: #98971a;}
    .sk_theme .annotation {color: #b16286;}
    .sk_theme .omnibar_highlight {color: #3c3836;}
    .sk_theme #sk_omnibarSearchResult ul li:nth-child(odd) {background: #fbf1c7;}
    .sk_theme #sk_omnibarSearchResult ul li.focused {background: #ebdbb2;}
    #sk_status, #sk_find {font-size: 20px;}
  `;
};

export const applyTheme = () => {
  applyDarkTheme();
};
