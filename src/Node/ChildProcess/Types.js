export const fromKillSignalImpl = (left, right, sig) => {
  const ty = typeof sig;
  if (ty === "number") return right(sig | 0);
  if (ty === "string") return left(sig);
  throw new Error("Impossible. Got kill signal that was neither int nor string: " + sig);
};
