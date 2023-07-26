export const showKillSignal = (ks) => ks + "";
export const showShell = (shell) => shell + "";
export const fromKillSignalImpl = (fromInt, fromStr, sig) => {
  const ty = typeof sig;
  if (ty === "number") return fromInt(sig | 0);
  if (ty === "string") return fromStr(sig);
  throw new Error("Impossible. Got kill signal that was neither int nor string: " + sig);
};
