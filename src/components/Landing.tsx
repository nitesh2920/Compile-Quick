import  { useEffect, useState } from "react";
import CodeEditor from "./CodeEditor";
import axios from "axios";
import {cn} from "../utils/cn";
import { languageOptions } from "../constants/languageOptions";
import type { LanguageOption } from "../constants/languageOptions";

import { ToastContainer, toast } from "react-toastify";
import "react-toastify/dist/ReactToastify.css";
import type { ThemeOption } from "./ThemeDropdown";
import { defineTheme } from "../lib/defineTheme";
import useKeyPress from "../hooks/useKeyPress";
import OutputWindow from "./OutputWindow";
import CustomInput from "./CustomInput";
import OutputDetails from "./OutputDetail";
import ThemeDropdown from "./ThemeDropdown";
import LanguagesDropdown from "./LanguagesDropDown";
import { defaultCodeSnippets } from "../constants/defaultCode";  // adjust the path as needed


// const javascriptDefault = `/**
// * Problem: Binary Search: Search a sorted array for a target value.
// */

// // Time: O(log n)
// const binarySearch = (arr, target) => {
//  return binarySearchHelper(arr, target, 0, arr.length - 1);
// };

// const binarySearchHelper = (arr, target, start, end) => {
//  if (start > end) {
//    return false;
//  }
//  let mid = Math.floor((start + end) / 2);
//  if (arr[mid] === target) {
//    return mid;
//  }
//  if (arr[mid] < target) {
//    return binarySearchHelper(arr, target, mid + 1, end);
//  }
//  if (arr[mid] > target) {
//    return binarySearchHelper(arr, target, start, mid - 1);
//  }
// };

// const arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
// const target = 5;
// console.log(binarySearch(arr, target));
// `;

interface OutputDetailsType {
  status?: { id: number; description?: string };
  memory?: string | number;
  time?: string | number;
  compile_output?: string;
  stdout?: string;
  stderr?: string;
}

const Landing: React.FC = () => {
  const [code, setCode] = useState<string>(defaultCodeSnippets.javascript);
  const [customInput, setCustomInput] = useState<string>("");
  const [outputDetails, setOutputDetails] = useState<OutputDetailsType | null>(null);
  const [processing, setProcessing] = useState<boolean>(false);
  const [theme, setTheme] = useState<ThemeOption | null>({
  value: "cobalt",
  label: "Cobalt",
  key: "cobalt",
});
  const [language, setLanguage] = useState(languageOptions[0]);

  const enterPress = useKeyPress("Enter");
  const ctrlPress = useKeyPress("Control");

  const onSelectChange = (selected: LanguageOption | null) => {
    if (selected) {
      setLanguage(selected);
      setCode(defaultCodeSnippets[selected.value]||"Enter your code here...")
      console.log("selected Option...", selected);
        console.log("Default snippet found:", defaultCodeSnippets[selected.value]);

    }
  };

  useEffect(() => {
    if (enterPress && ctrlPress) {
      handleCompile();
    }
  }, [ctrlPress, enterPress]);

  const onChange = (action: string, data: string) => {
    if (action === "code") {
      setCode(data);
    } else {
      console.warn("case not handled!", action, data);
    }
  };

  const handleCompile = () => {
    console.log("handleCompile called..."); 
    setProcessing(true);
    const formData = {
      language_id: language.id,
      source_code: btoa(code),
      stdin: btoa(customInput),
    };

    const options = {
      method: "POST",
      url: import.meta.env.VITE_APP_RAPID_API_URL,
      params: { base64_encoded: "true", fields: "*" },
      headers: {
        "content-type": "application/json",
        "X-RapidAPI-Host": import.meta.env.VITE_APP_RAPID_API_HOST ?? "",
        "X-RapidAPI-Key": import.meta.env.VITE_APP_RAPID_API_KEY ?? "",
      },
      data: formData,
    };

    axios
      .request(options)
      .then((response) => {
        const token = response.data.token;
        checkStatus(token);
      })
      .catch((err) => {
        const error = err.response ? err.response.data : err;
        const status = err.response?.status;

        if (status === 429) {
          showErrorToast(
            `Quota of 100 requests exceeded for the Day! Please read the blog on freeCodeCamp to learn how to setup your own RAPID API Judge0!`,
            10000
          );
        }
        setProcessing(false);
        console.error("catch block...", error);
      });
  };

  const checkStatus = async (token: string) => {
    const options = {
      method: "GET",
      url: `${import.meta.env.VITE_APP_RAPID_API_URL}/${token}`,
      params: { base64_encoded: "true", fields: "*" },
      headers: {
        "X-RapidAPI-Host": import.meta.env.VITE_APP_RAPID_API_HOST ?? "",
        "X-RapidAPI-Key": import.meta.env.VITE_APP_RAPID_API_KEY ?? "",
      },
    };
    try {
      const response = await axios.request(options);
      const statusId = response.data.status?.id;

      if (statusId === 1 || statusId === 2) {
        setTimeout(() => checkStatus(token), 2000);
        return;
      } else {
        setProcessing(false);
        setOutputDetails(response.data);
        showSuccessToast("Compiled Successfully!");
      }
    } catch (err) {
      setProcessing(false);
      showErrorToast();
      console.error("err", err);
    }
  };

 function handleThemeChange(th: ThemeOption | null) {
  console.log("handleThemeChange called with:", th);
  if (!th) return;

  const themeObj = {
    value: th.value,
    label: th.label ?? th.value,
    key: th.value,
  };

  if (["light", "vs-dark"].includes(th.value)) {
    setTheme(themeObj);
  } else {
    defineTheme(th.value)
      .then(() => setTheme(themeObj))
      .catch((e) => {
        console.error("Error defining theme:", e);
        // fallback to default theme maybe?
      });
  }
}


  useEffect(() => {
    defineTheme("oceanic-next").then(() =>
      setTheme({ value: "oceanic-next", label: "Oceanic Next",key: "oceanic-next" })
    );
  }, []);

  const showSuccessToast = (msg?: string) => {
    toast.success(msg ?? "Compiled Successfully!", {
      position: "top-right",
      autoClose: 1000,
      hideProgressBar: false,
      closeOnClick: true,
      pauseOnHover: true,
      draggable: true,
    });
  };

  const showErrorToast = (msg?: string, timer?: number) => {
    toast.error(msg ?? "Something went wrong! Please try again.", {
      position: "top-right",
      autoClose: timer ?? 1000,
      hideProgressBar: false,
      closeOnClick: true,
      pauseOnHover: true,
      draggable: true,
    });
  };

  return (
    <>
      <ToastContainer
        position="top-right"
        autoClose={2000}
        hideProgressBar={false}
        newestOnTop={false}
        closeOnClick
        rtl={false}
        pauseOnFocusLoss
        draggable
        pauseOnHover
      />

    
      <div className="h-4 w-full bg-gradient-to-r from-purple-700 via-purple-900 to-purple-800"></div>
      <div className="flex flex-row">
        <div className="px-4 py-2">
          <LanguagesDropdown onSelectChange={onSelectChange} />
        </div>
        <div className="px-4 py-2">
          <ThemeDropdown handleThemeChange={handleThemeChange} theme={theme} />
        </div>
      </div>
      <div className="flex flex-row space-x-4 items-start px-4 py-4 bg-gray-900 text-purple-200 min-h-[80vh]">
        <div className="flex flex-col w-full h-full justify-start items-end">
          <CodeEditor
          key={language.value} // Ensure the editor re-renders on language change
            code={code}
            onChange={onChange}
            language={language?.value}
            theme={theme?.value ?? "light"}
          />
        </div>

        <div className="right-container flex flex-shrink-0 w-[30%] flex-col space-y-4">
          <OutputWindow outputDetails={outputDetails} />
          <div className="flex flex-col items-end">
            <CustomInput customInput={customInput} setCustomInput={setCustomInput} />
            <button
              onClick={handleCompile}
              disabled={!code}
              className={cn(
                "mt-4 border-2 border-purple-500 rounded-md shadow-[5px_5px_0px_0px_rgba(128,0,128,0.8)] px-4 py-2 hover:shadow transition duration-200 bg-purple-700 text-white flex-shrink-0",
                !code ? "opacity-50" : ""
              )}
            >
              {processing ? "Processing..." : "Compile and Execute"}
            </button>
          </div>
          {outputDetails && <OutputDetails outputDetails={outputDetails} />}
        </div>
      </div>
     
    </>
  );
};

export default Landing;
