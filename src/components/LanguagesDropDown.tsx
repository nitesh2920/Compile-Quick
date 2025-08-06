import { languageOptions } from "../constants/languageOptions";
import type { LanguageOption } from "../constants/languageOptions";
import Select from "react-select";
import { customStyles } from "../constants/customStyles";


interface LanguagesDropDownProps {
  onSelectChange: (selectedOption: LanguageOption | null) => void;
}

const LanguagesDropDown : React.FC<LanguagesDropDownProps>= ({onSelectChange}) => {
    return(
        <Select 
        placeholder="Select Language"
        options={languageOptions}
        defaultValue={languageOptions[0]}
        styles={customStyles}
        onChange={(option) => onSelectChange(option as LanguageOption | null)}
        />
    )
}

export default LanguagesDropDown;