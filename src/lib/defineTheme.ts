import { loader } from "@monaco-editor/react";
import type * as monacoEditor from "monaco-editor";
import monacoThemes from "../constants/themeList";

const themeModules = import.meta.glob("../themes/*.json", { query: "?json" });

export const defineTheme = async (theme: string): Promise<void> => {
  await loader.init();

  const themeFileName = monacoThemes[theme];
  if (!themeFileName) {
    throw new Error(`Theme "${theme}" not defined in theme list.`);
  }

  const importPath = `../themes/${themeFileName}.json`;
  const importer = themeModules[importPath];
  if (!importer) {
    throw new Error(`Theme file not found: ${importPath}`);
  }

  const themeData = (await importer()) as monacoEditor.editor.IStandaloneThemeData;
  const monaco = await loader.__getMonacoInstance();

  if (!monaco) throw new Error("Monaco not initialized");

  monaco.editor.defineTheme(theme, themeData);
  monaco.editor.setTheme(theme);
};
