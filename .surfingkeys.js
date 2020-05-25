settings.tabsThreshold = 0;
settings.scrollStepSize = 140;
settings.focusFirstCandidate = true;
settings.hintAlign = 'left';

Hints.characters = 'yuiophjklnm'; // for right hand
Hints.style('color: black; font-family: sans-serif;');


const getElement = (selector) => getElements(selector)[0];

const dispatchMouseClicks = (elements) => elements.forEach((element) => Hints.dispatchMouseClick(element));

const openStoryAndComments = ({story, link, comments}) => {
    Hints.create(story, (storyElement) => {
      dispatchMouseClicks([
        getElement(`*[id="${storyElement.id}"] ${link}`),
        getElement(`*[id="${storyElement.id}"] ${comments}`)
      ]);
    }, {multipleHits: true});
};

map('F', 'gf');
mapkey('gf', '#1Open Link and Comments [Lobsters]', () => openStoryAndComments({
    story: 'li.story',
    link: 'a.u-url',
    comments: '.comments_label a'
  }) , {domain: /lobste\.rs/});

mapkey('gf', '#1Open Link and Comments [Lobsters]', () => openStoryAndComments({
    story: 'tr.athing',
    link: 'a.storylink',
    comments: '+ tr td.subtext > a:last-of-type'
  }) , {domain: /news\.ycombinator\.com/});

// gruvbox dark
settings.theme = `
.sk_theme {
    font-family: Input Sans Condensed, Charcoal, sans-serif;
    font-size: 10pt;
    background: #282828;
    color: #ebdbb2;
}
.sk_theme tbody {
    color: #b8bb26;
}
.sk_theme input {
    color: #d9dce0;
}
.sk_theme .url {
    color: #98971a;
}
.sk_theme .annotation {
    color: #b16286;
}
.sk_theme .omnibar_highlight {
    color: #ebdbb2;
}
.sk_theme #sk_omnibarSearchResult ul li:nth-child(odd) {
    background: #282828;
}
.sk_theme #sk_omnibarSearchResult ul li.focused {
    background: #3c3836;
}
#sk_status, #sk_find {
    font-size: 20pt;
}`;
