export const getStdin = async () => {
  let chunks = [];

  for await (const chunk of process.stdin) {
    chunks.push(chunk);
  }

  return chunks.join("");
};

export const toLines = (str: string) => str.split("\n").map((l) => l.trim());

export const getInputLines = async () => toLines(await getStdin());
