type Selector = string;

export const chars = ([chars]: TemplateStringsArray) => chars.split('');
export const words = ([words]: TemplateStringsArray) => words.split(' ');

export const removeSticky = ({
  root,
  nice,
}: {
  root: Document | DocumentFragment;
  nice: boolean;
}) => {
  for (let element of Array.from(root.querySelectorAll('*'))) {
    if (element.shadowRoot) {
      removeSticky({ root: element.shadowRoot, nice: nice });
    }

    if (element instanceof HTMLElement) {
      if (isFixedOrSticky(element) && element.className !== 'sk_ui') {
        if (nice) {
          if (isHorizontal(element)) {
            if (isFixed(element)) {
              element.style.position = 'absolute';
            }
            if (isSticky(element)) {
              element.style.position = 'relative';
            }
            if (element.style.top !== '') {
              element.style.top = '0';
            }
            console.log('unstickied', element, element.style.position);
          }
        } else {
          element.parentNode?.removeChild(element);
          console.log('removed', element);
        }
      }
    }
  }
};

const isHorizontal = (element: HTMLElement) => {
  let rect = element.getBoundingClientRect();
  return rect.width / rect.height > 2;
};

const isFixed = (element: HTMLElement) => {
  let style = getComputedStyle(element);
  return style.position === 'fixed';
};

const isSticky = (element: HTMLElement) => {
  let style = getComputedStyle(element);
  return style.position === 'sticky';
};

const isFixedOrSticky = (element: HTMLElement) =>
  isFixed(element) || isSticky(element);

const getElement = (selector: Selector) =>
  api.getClickableElements(selector)[0];

// const dispatchMouseClicks = (elements: HTMLAnchorElement[]) =>
//   new Set(elements.map(element => element.href)).forEach(href =>
//     api.RUNTIME('openLink', {
//       tab: {
//         tabbed: true,
//         active: false,
//       },
//       url: href,
//     })
//   );

const dispatchMouseClicks = (elements: HTMLAnchorElement[]) =>
  new Set(elements).forEach(element => api.Hints.dispatchMouseClick(element));

export const openStoryAndComments = ({
  story,
  link,
  comments,
}: {
  story: Selector;
  link: Selector;
  comments: Selector;
}) => {
  api.Hints.create(
    story,
    (storyElement: HTMLElement) => {
      dispatchMouseClicks([
        getElement(`*[id="${storyElement.id}"] ${link}`) as HTMLAnchorElement,
        getElement(
          `*[id="${storyElement.id}"] ${comments}`
        ) as HTMLAnchorElement,
      ]);
    },
    { active: false, tabbed: true, multipleHits: false }
  );
};

export const createSuggestionItem = (html: string, props = {}) => {
  const li = document.createElement('li');
  li.innerHTML = html;
  return { html: li.outerHTML, props };
};

const withScrollingIndicator = (amount: number, fn: () => void) => {
  let line: HTMLElement | null = document.querySelector('#scrollingIndicator');
  if (line == null) {
    line = document.createElement('div');

    line.id = 'scrollingIndicator';
    line.style.cssText = `
      mix-blend-mode: screen;
      position: absolute;
      width: 100%;
      height: 10px;
      background: #ff0000;
      z-index: 9999;
    `;
  }

  line.style.top = `${window.scrollY -
    10 * amount +
    (amount < 0 ? 0 : amount * window.innerHeight)}px`;

  document.body.appendChild(line);
  window.addEventListener('scrollend', removeScrollingIndicators);
  removeScrollingIndicators();

  fn();
};

function removeScrollingIndicators() {
  document.querySelectorAll('#scrollingIndicator').forEach(e => e.remove());
}

export const scrollBy = (amount: number) =>
  withScrollingIndicator(amount, () =>
    document.scrollingElement?.scrollBy({
      behavior: 'smooth',
      left: 0,
      top: amount * window.innerHeight,
    })
  );

type FieldSpec = { sel?: string; re?: RegExp };

type GaugesSpec = {
  rows: string;
  anchor: string;
  score: FieldSpec;
  comments: FieldSpec;
};

const GAUGE_STYLE_ID = 'surfingkeys-gauges';

const GAUGE_CSS = `
  .gauge         { width: 0%; }
  .gauge-score   { border-bottom: 2px solid #fabd2f; }
  .gauge-comment { border-top:    2px solid #fb4934; margin-bottom: 1ex; }
  .gauge-1  { width: 1%;  }
  .gauge-2  { width: 2%;  }
  .gauge-3  { width: 3%;  }
  .gauge-4  { width: 5%;  }
  .gauge-5  { width: 8%;  }
  .gauge-6  { width: 13%; }
  .gauge-7  { width: 21%; }
  .gauge-8  { width: 34%; }
  .gauge-9  { width: 55%; }
  .gauge-10 { width: 89%; }
`;

const injectGaugeStyles = () => {
  if (document.getElementById(GAUGE_STYLE_ID)) return;
  const style = document.createElement('style');
  style.id = GAUGE_STYLE_ID;
  style.textContent = GAUGE_CSS;
  document.head.appendChild(style);
};

const bucket = (n: number): string =>
  Number(n)
    .toString(Math.E)
    .length.toString();

const extractNumber = (row: Element, field: FieldSpec): number | null => {
  const source = field.sel ? row.querySelector(field.sel) : row;
  if (!source) return null;
  const text = (source as HTMLElement).innerText;

  if (field.re) {
    const match = text.match(field.re);
    if (!match) return null;
    const n = Number(match[1]);
    return Number.isFinite(n) ? n : null;
  }
  const n = Number(text.trim());
  return Number.isFinite(n) ? n : null;
};

const insertGauge = (target: Element, kind: string, n: number) =>
  target.insertAdjacentHTML(
    'beforebegin',
    `<div class="gauge ${kind} gauge-${bucket(n)}"></div>`
  );

export const renderGauges = ({
  rows,
  anchor,
  score,
  comments,
}: GaugesSpec) => {
  injectGaugeStyles();
  document.querySelectorAll(rows).forEach(row => {
    const target = row.querySelector(anchor);
    if (!target) return;

    const scoreValue = extractNumber(row, score);
    if (scoreValue !== null) insertGauge(target, 'gauge-score', scoreValue);

    const commentsValue = extractNumber(row, comments);
    if (commentsValue !== null)
      insertGauge(target, 'gauge-comment', commentsValue);
  });
};

export const darkReaderEnabled = () =>
  document.querySelector('style.darkreader') ||
  document.querySelector('style#dark-reader-style');

export const defaultIcon = `
data:image/svg+xml,%3C%3Fxml%20version%3D%221.0%22%20encoding%3D%22UTF-8%22%3F%3E%0A%3Csvg%20en
able-background%3D%22new%200%200%2056%2056%22%20viewBox%3D%220%200%2056%2056
%22%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%3E%0A%3Cpath%20d%3D
%22m0%200h56v56h-56z%22%20fill%3D%22%23eee%22%2F%3E%0A%3Cpath%20d%3D%22M36.4
%2013.5h-18.6v24.9c0%201.4.9%202.3%202.3%202.3h18.7v-25c.1-1.4-1-2.2-2.4-2.2
zm-6.2%203.5h5.1v6.4h-5.1v-6.4zm-8.8%200h6v1.8h-6v-1.8zm0%204.6h6v1.8h-6v-1.
8zm0%2015.5v-1.8h13.8v1.8h-13.8zm13.8-4.5h-13.8v-1.8h13.8v1.8zm0-4.7h-13.8v-
1.8h13.8v1.8z%22%20fill%3D%22%23999%22%2F%3E%0A%3C%2Fsvg%3E`.replace(
  /\s+/g,
  ''
);

export const stayHere = () =>
  window.addEventListener('beforeunload', function(e) {
    e.preventDefault();
  });
