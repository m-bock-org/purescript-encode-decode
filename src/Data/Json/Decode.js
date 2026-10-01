// A frame read with every number kept as the text it was written as.
//
// `JSON.parse` hands a reviver the source text of each primitive in
// `context.source` (Node 21 on, current browsers). Where a runtime does
// not, the text is refused rather than read through a double: a silently
// rounded amount is the one outcome this parser exists to rule out.

/**
 * @typedef {(key: string, value: unknown, context?: { source?: unknown }) => unknown} Reviver
 * @typedef {(text: string, reviver: Reviver) => unknown} ParseWithSource
 */
const parseWithSource = /** @type {ParseWithSource} */ (/** @type {unknown} */ (JSON.parse));

/** @type {(left: (why: string) => unknown, right: (json: unknown) => unknown, raw: string) => unknown} */
export const parseKeepingNumbersImpl = (left, right, raw) => {
  let withoutSource = false;
  let parsed;
  try {
    parsed = parseWithSource(raw, (_key, value, context) => {
      if (typeof value !== "number") return value;
      if (context === undefined || typeof context.source !== "string") {
        withoutSource = true;
        return value;
      }
      return context.source;
    });
  } catch (why) {
    return left(why instanceof Error ? why.message : String(why));
  }
  if (withoutSource) return left("this runtime does not give JSON.parse the text of a number");
  return right(parsed);
};
