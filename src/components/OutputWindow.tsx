import React from "react";

interface OutputDetails {
  status?: {
    id: number;
  };
  compile_output?: string;
  stdout?: string;
  stderr?: string;
}

interface OutputWindowProps {
  outputDetails?: OutputDetails | null;
}

const decodeBase64 = (str?: string): string => {
  try {
    return str ? atob(str) : "";
  } catch {
    return "Error decoding output.";
  }
};

const OutputWindow: React.FC<OutputWindowProps> = ({ outputDetails }) => {
  const getOutput = () => {
    const statusId = outputDetails?.status?.id;

    if (statusId === 6) {
      // compilation error
      return (
        <pre className="px-3 py-2 font-mono text-xs text-red-400 bg-gradient-to-r from-purple-900 via-purple-800 to-purple-700 rounded-md shadow-md">
          {decodeBase64(outputDetails?.compile_output)}
        </pre>
      );
    } else if (statusId === 3) {
      const decodedStdout = decodeBase64(outputDetails?.stdout);
      return (
        <pre className="px-3 py-2 font-mono text-xs text-green-400 bg-gradient-to-r from-purple-900 via-purple-800 to-purple-700 rounded-md shadow-md">
          {decodedStdout || "No output"}
        </pre>
      );
    } else if (statusId === 5) {
      return (
        <pre className="px-3 py-2 font-mono text-xs text-yellow-400 bg-gradient-to-r from-purple-900 via-purple-800 to-purple-700 rounded-md shadow-md">
          Time Limit Exceeded
        </pre>
      );
    } else {
      return (
        <pre className="px-3 py-2 font-mono text-xs text-red-400 bg-gradient-to-r from-purple-900 via-purple-800 to-purple-700 rounded-md shadow-md">
          {decodeBase64(outputDetails?.stderr)}
        </pre>
      );
    }
  };

  return (
    <>
      <h1 className="mb-3 text-center sm:text-start mt-3 sm:mt-0 text-2xl font-extrabold bg-gradient-to-r from-purple-400 via-pink-500 to-red-500 bg-clip-text text-transparent">
        Output
      </h1>
      <div className="w-full max-w-xl h-48 bg-gradient-to-br from-black via-purple-900 to-purple-800 rounded-lg p-4 overflow-auto shadow-xl border border-purple-700">
        {outputDetails ? getOutput() : (
          <p className="text-gray-400 italic font-mono text-sm">No output yet.</p>
        )}
      </div>
    </>
  );
};

export default OutputWindow;
