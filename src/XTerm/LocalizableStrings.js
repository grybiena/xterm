export const getPromptLabel = l => () => l.promptLabel;
export const setPromptLabel = l => s => () => l.promptLabel = s;

export const getTooMuchOutput = l => () => l.tooMuchOutput;
export const setTooMuchOutput = l => s => () => l.tooMuchOutput = s;

