export const _webGlAddon = () => {
  try {
    return new window.WebglAddon.WebglAddon();
  } catch (e) {
    console.warn('WebGL Addon not available.', e);
  }
}
