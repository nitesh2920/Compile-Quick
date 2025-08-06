import React from "react";

interface Status {
  description?: string;
}

interface OutputDetailsProps {
  outputDetails?: {
    status?: Status;
    memory?: string | number;
    time?: string | number;
  } | null;
}

const OutputDetails: React.FC<OutputDetailsProps> = ({ outputDetails }) => {
  return (
    <div className="metrics-container mt-4 flex flex-col space-y-3 bg-gradient-to-br from-purple-900 via-indigo-900 to-indigo-800 p-4 rounded-md shadow-lg text-gray-100">
      <p className="text-sm">
        Status:{" "}
        <span className="font-semibold px-2 py-1 rounded-md bg-gradient-to-r from-purple-700 via-purple-600 to-purple-500 text-white">
          {outputDetails?.status?.description ?? "N/A"}
        </span>
      </p>
      <p className="text-sm">
        Memory:{" "}
        <span className="font-semibold px-2 py-1 rounded-md bg-gradient-to-r from-purple-700 via-purple-600 to-purple-500 text-white">
          {outputDetails?.memory ?? "N/A"}
        </span>
      </p>
      <p className="text-sm">
        Time:{" "}
        <span className="font-semibold px-2 py-1 rounded-md bg-gradient-to-r from-purple-700 via-purple-600 to-purple-500 text-white">
          {outputDetails?.time ?? "N/A"}
        </span>
      </p>
    </div>
  );
};

export default OutputDetails;
