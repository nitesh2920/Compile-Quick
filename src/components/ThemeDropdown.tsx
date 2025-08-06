import React from "react";
import Select from "react-select";
import type { SingleValue } from "react-select";
import { customStyles } from "../constants/customStyles";
import type { StylesConfig } from "react-select";
import monacoThemes from "../constants/themeList"

export interface ThemeOption {
  label: string;
  value: string;
  key: string;
}

interface ThemeDropdownProps {
  theme: ThemeOption | null;
  handleThemeChange: (selectedOption: SingleValue<ThemeOption>) => void;
}

const ThemeDropdown: React.FC<ThemeDropdownProps> = ({ handleThemeChange, theme }) => {
  const options: ThemeOption[] = Object.entries(monacoThemes).map(([themeId, themeName]) => ({
    label: themeName,
    value: themeId,
    key: themeId,
  }));
  return (
    <Select<ThemeOption, false>
      placeholder={`Select Theme`}
      options={options}
      value={theme}
      styles={customStyles as StylesConfig<ThemeOption, false>}
      onChange={handleThemeChange}
      isClearable
    />
  );
};

export default ThemeDropdown;
