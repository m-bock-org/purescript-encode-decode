// A JSON number written from its digits: `JSON.stringify` writes a
// `JSON.rawJSON` value as the text it holds (Node 21 on, current
// browsers), so the digits never pass through a double.

/** @type {(digits: string) => unknown} */
export const rawNumberImpl = (digits) =>
  /** @type {{ rawJSON: (text: string) => unknown }} */ (/** @type {unknown} */ (JSON)).rawJSON(digits);
