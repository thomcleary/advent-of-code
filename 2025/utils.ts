export const stdin = async () => {
  let chunks = [];

  for await (const chunk of process.stdin) {
    chunks.push(chunk);
  }

  return chunks.join("").trim();
};
