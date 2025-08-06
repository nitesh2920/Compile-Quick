import { useEffect, useState } from "react";
import Editor from "@monaco-editor/react";

interface CodeEditorProps {
  onChange: (action: string, value: string) => void;
  language: string;
  code: string;
  theme: string;
}

const CodeEditor: React.FC<CodeEditorProps> = ({
  onChange,
  language,
  code,
  theme
}) => {
  const [value, setValue] = useState<string>(code || "");

useEffect(()=>{
  setValue(code)

},[code])

  const handleEditorChange=(newValue:string|undefined)=>{
    const updatedValue=newValue||"";
    setValue(updatedValue);
    onChange("code",updatedValue);

  }

  return (
    <div className="overlay rounded-md overflow-hidden w-full h-full shadow-4xl">
      <Editor
        height="85vh"
        width="100%"
        language={language || "javascript"}
        value={value}
        theme={theme}
        defaultValue="// some comment"
        onChange={handleEditorChange}
      />
    </div>
  );
};

export default CodeEditor;
