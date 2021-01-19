type Selector = string;

export const chars = ([chars]: TemplateStringsArray) => chars.split('');
export const words = ([words]: TemplateStringsArray) => words.split(' ');

export const removeSticky = () => {
  let position;
  for (let element of Array.from(document.querySelectorAll('body *'))) {
    position = getComputedStyle(element).position;
    if (position === 'fixed' || position === 'sticky') {
      element.parentNode?.removeChild(element);
    }
  }
};

const getElement = (selector: Selector) => getElements(selector)[0];

const dispatchMouseClicks = (elements: HTMLAnchorElement[]) =>
  elements.forEach(element => RUNTIME("openLink", {
    tab: {
      tabbed: true,
      active: false
    },
    url: element.href
  }));

export const openStoryAndComments = ({
  story,
  link,
  comments,
}: {
  story: Selector;
  link: Selector;
  comments: Selector;
}) => {
  Hints.create(
    story,
    (storyElement: HTMLElement) => {
      dispatchMouseClicks([
        getElement(`*[id="${storyElement.id}"] ${link}`) as HTMLAnchorElement,
        getElement(`*[id="${storyElement.id}"] ${comments}`) as HTMLAnchorElement,
      ]);
    }
  );
};

export const createSuggestionItem = (html: string, props = {}) => {
  const li = document.createElement('li');
  li.innerHTML = html;
  return { html: li.outerHTML, props };
};
