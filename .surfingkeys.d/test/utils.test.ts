import { renderGauges, widthPercent } from '../src/utils';

const HN_SPEC = {
  rows: 'td.subtext',
  anchor: 'span.subline',
  score: { re: /(\d+)\spoints/, k: 100 },
  comments: { re: /(\d+)\scomments/, k: 30 },
};

const buildHnRow = (subtextLine: string) => {
  while (document.body.firstChild) {
    document.body.removeChild(document.body.firstChild);
  }
  const table = document.createElement('table');
  const tr = document.createElement('tr');
  const td = document.createElement('td');
  td.className = 'subtext';
  const span = document.createElement('span');
  span.className = 'subline';
  span.textContent = subtextLine;
  td.appendChild(span);
  tr.appendChild(td);
  table.appendChild(tr);
  document.body.appendChild(table);
};

describe('widthPercent (Hill scaling)', () => {
  it('returns 0 for value=0', () => {
    expect(widthPercent(0, 10)).toBe(0);
  });

  it('returns exactly 50 when value equals k', () => {
    expect(widthPercent(10, 10)).toBe(50);
    expect(widthPercent(100, 100)).toBe(50);
    expect(widthPercent(1, 1)).toBe(50);
  });

  it('returns ~75 when value is 3x k', () => {
    expect(widthPercent(30, 10)).toBeCloseTo(75, 5);
  });

  it('returns exactly 90 when value is 9x k', () => {
    expect(widthPercent(90, 10)).toBe(90);
  });

  it('clamps negative values to 0', () => {
    expect(widthPercent(-5, 10)).toBe(0);
    expect(widthPercent(-1000, 100)).toBe(0);
  });

  it('asymptotes to but never reaches 100 for very large values', () => {
    const w = widthPercent(1e9, 10);
    expect(w).toBeLessThan(100);
    expect(w).toBeGreaterThan(99.999);
  });

  it('returns 0 when k <= 0', () => {
    expect(widthPercent(5, 0)).toBe(0);
    expect(widthPercent(5, -1)).toBe(0);
  });

  it('is monotonically non-decreasing in value', () => {
    const k = 50;
    let prev = widthPercent(0, k);
    for (const v of [1, 10, 25, 50, 100, 500, 5000]) {
      const cur = widthPercent(v, k);
      expect(cur).toBeGreaterThanOrEqual(prev);
      prev = cur;
    }
  });
});

describe('renderGauges readiness guard', () => {
  let readyStateValue: DocumentReadyState = 'complete';
  let originalDescriptor: PropertyDescriptor | undefined;

  beforeAll(() => {
    originalDescriptor = Object.getOwnPropertyDescriptor(
      Document.prototype,
      'readyState'
    );
    Object.defineProperty(document, 'readyState', {
      configurable: true,
      get: () => readyStateValue,
    });
  });

  afterAll(() => {
    if (originalDescriptor) {
      Object.defineProperty(
        Document.prototype,
        'readyState',
        originalDescriptor
      );
    }
  });

  beforeEach(() => {
    buildHnRow('86 points 14 comments');
    document.getElementById('surfingkeys-gauges')?.remove();
  });

  it('defers rendering when document is still loading', () => {
    readyStateValue = 'loading';

    renderGauges(HN_SPEC);

    expect(document.querySelectorAll('.gauge').length).toBe(0);

    document.dispatchEvent(new Event('DOMContentLoaded'));

    expect(document.querySelectorAll('.gauge').length).toBe(2);
  });

  it('renders immediately when document is interactive', () => {
    readyStateValue = 'interactive';

    renderGauges(HN_SPEC);

    expect(document.querySelectorAll('.gauge').length).toBe(2);
  });

  it('renders immediately when document is complete', () => {
    readyStateValue = 'complete';

    renderGauges(HN_SPEC);

    expect(document.querySelectorAll('.gauge').length).toBe(2);
  });
});
