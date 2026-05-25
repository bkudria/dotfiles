import { GAUGE_PALETTE, applyGaugeVars } from '../src/theme';

describe('GAUGE_PALETTE', () => {
  it('exposes gruvbox bright colors for dark mode', () => {
    expect(GAUGE_PALETTE.dark.score).toEqual('#fabd2f');
    expect(GAUGE_PALETTE.dark.comment).toEqual('#fb4934');
  });

  it('exposes gruvbox faded colors for light mode', () => {
    expect(GAUGE_PALETTE.light.score).toEqual('#b57614');
    expect(GAUGE_PALETTE.light.comment).toEqual('#9d0006');
  });
});

describe('applyGaugeVars', () => {
  const STYLE_ID = 'surfingkeys-theme-vars';

  beforeEach(() => {
    document.getElementById(STYLE_ID)?.remove();
  });

  const styleContent = () =>
    document.getElementById(STYLE_ID)?.textContent ?? '';

  it('injects a single :root style block with dark gauge variables', () => {
    applyGaugeVars('dark');
    expect(document.querySelectorAll(`#${STYLE_ID}`).length).toBe(1);
    expect(styleContent()).toContain('--gauge-score-color: #fabd2f');
    expect(styleContent()).toContain('--gauge-comment-color: #fb4934');
    expect(styleContent()).toMatch(/:root\s*{/);
  });

  it('injects light gauge variables when mode is light', () => {
    applyGaugeVars('light');
    expect(styleContent()).toContain('--gauge-score-color: #b57614');
    expect(styleContent()).toContain('--gauge-comment-color: #9d0006');
  });

  it('replaces previously-set variables when called again without duplicating the style block', () => {
    applyGaugeVars('dark');
    applyGaugeVars('light');
    expect(document.querySelectorAll(`#${STYLE_ID}`).length).toBe(1);
    expect(styleContent()).toContain('--gauge-score-color: #b57614');
    expect(styleContent()).toContain('--gauge-comment-color: #9d0006');
    expect(styleContent()).not.toContain('#fabd2f');
  });
});
