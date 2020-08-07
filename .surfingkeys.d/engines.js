const wpDefaultIcon =
  "data:image/svg+xml,%3C%3Fxml%20version%3D%221.0%22%20encoding%3D%22utf-8%22%3F%3E%0A%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20viewBox%3D%220%200%2056%2056%22%20enable-background%3D%22new%200%200%2056%2056%22%3E%0A%20%20%20%20%3Cpath%20fill%3D%22%23eee%22%20d%3D%22M0%200h56v56h-56z%22%2F%3E%0A%20%20%20%20%3Cpath%20fill%3D%22%23999%22%20d%3D%22M36.4%2013.5h-18.6v24.9c0%201.4.9%202.3%202.3%202.3h18.7v-25c.1-1.4-1-2.2-2.4-2.2zm-6.2%203.5h5.1v6.4h-5.1v-6.4zm-8.8%200h6v1.8h-6v-1.8zm0%204.6h6v1.8h-6v-1.8zm0%2015.5v-1.8h13.8v1.8h-13.8zm13.8-4.5h-13.8v-1.8h13.8v1.8zm0-4.7h-13.8v-1.8h13.8v1.8z%22%2F%3E%0A%3C%2Fsvg%3E%0A";

const createSuggestionItem = (html, props = {}) => {
  const li = document.createElement("li");
  li.innerHTML = html;
  return { html: li.outerHTML, props };
};

chars`bdghwyse`.forEach((searchAlias) => {
  removeSearchAliasX(searchAlias, "s");
  removeSearchAliasX(searchAlias, "o");
});

const wikipediaCallback = (response) =>
  Object.values(JSON.parse(response.text).query.pages).map((p) => {
    const img = p.thumbnail ? p.thumbnail.source : wpDefaultIcon;
    const desc = p.description ? p.description : "";
    return createSuggestionItem(
      `<div style="padding:5px;display:grid;grid-template-columns:60px 1fr;grid-gap:15px">
              <img style="width:60px" src="${img}" alt="${p.title}">
              <div>
                <div class="title"><strong>${p.title}</strong></div>
                <div class="title">${desc}</div>
              </div>
            </div>`,
      { url: p.fullurl }
    );
  });

addSearchAliasX(
  "wi",
  "WP",
  "https://en.wikipedia.org/w/index.php?search=",
  "s",
  "https://en.wikipedia.org/w/api.php?action=query&format=json&generator=prefixsearch&prop=info|pageprops%7Cpageimages%7Cdescription&redirects=&ppprop=displaytitle&piprop=thumbnail&pithumbsize=100&pilimit=6&inprop=url&gpssearch=",
  wikipediaCallback
);

addSearchAliasX(
  "go",
  "Google",
  "https://www.google.com/search?q=",
  "s",
  "https://www.google.com/complete/search?client=chrome-omni&gs_ri=chrome-ext&oit=1&cp=1&pgcl=7&q=",
  (response) => JSON.parse(response.text)[1]
);

addSearchAliasX(
  "ddg",
  "DDG",
  "https://duckduckgo.com/?q=",
  "s",
  "https://duckduckgo.com/ac/?q=",
  (response) => JSON.parse(response.text).map((r) => r.phrase)
);

addSearchAliasX(
  "ddl",
  "DDG>",
  "https://duckduckgo.com/?q=\\",
  "s",
  "https://duckduckgo.com/ac/?q=\\",
  (response) => JSON.parse(response.text).map((r) => r.phrase)
);

mapkey("ow", "Search Wikipedia", () =>
  window.Front.openOmnibar({ type: "SearchEngine", extra: "wi" })
);
mapkey("os", "Search DDG", () =>
  window.Front.openOmnibar({ type: "SearchEngine", extra: "ddg" })
);
mapkey("ol", "Lucky Duck", () =>
  window.Front.openOmnibar({ type: "SearchEngine", extra: "ddl" })
);
