import shutil
import magic
import zipfile
import os

from openai import OpenAI
from pathlib import Path
from datetime import datetime
from PyPDF2 import PdfReader
from docx import Document
from dotenv import load_dotenv

# initialize folders
folders = ["School", "Extra", "Software", "Personal", "Pictures", "Videos", "Data"]

def get_folder(words, filename):
    prompt = f"""
    Filename: {filename}

    First 300 words:
    {words}

    Choose the best folder:
    - School
    - Software
    - Personal
    - Extra

    Return ONLY the folder name.
    """

    response = client.responses.create(
        model="gpt-5",
        instructions="""
        You are a file classification assistant. 
        
        Choose only one folder from the provided list. 
        Return ONLY the folder name
        """,
        input=prompt
    )

    folder = response.output_text.strip()

    if folder not in folders:
        return "Extra"
    return folder

visited = set()
# initialize client
load_dotenv()
key = os.getenv("OPENAI_API_KEY")

client = OpenAI(api_key=key)

# change directory to desired (Downloads)
downloads = Path.home() / "Downloads"

# iterate through files, first moving creating new folders by year then by desired cols
for file in downloads.iterdir():

    if not file.is_file():
        continue
    
    # collect timestamp, put into years folder
    timestamp = file.stat().st_mtime
    curr_year = datetime.fromtimestamp(timestamp).year

    year_folder = downloads / str(curr_year)
    year_folder.mkdir(exist_ok=True) # if folder already exists doesn't make it

    # Firstly move file into year folder
    shutil.move(str(file), str(year_folder))

    new_file = year_folder / file.name

    """
    Next part: 
    for the current file, go into the correct year folder, scan the file, then ask
    client to determine which folder it goes into

    Maybe initialize folders into each year folder
    """
    curr_folder = year_folder

    # Initializes other folders
    if curr_folder not in visited:
        for name in folders:
            path = curr_folder / name
            if not path.is_dir():
               path.mkdir(exist_ok=True)
        visited.add(curr_folder)
    
    file_type = magic.from_file(str(new_file))

    # Go through file types
    
    # Pictures
    pics = ["jpeg", "jpg", "png", "webp", "gif"]
    vids = ["mp4", "mov", "mkv", "wmv", "avi"]
    data = ["csv", "xlsx", "json", "xml", "yaml", "txt"]

    f_type = new_file.suffix.lower().replace(".", "")
    if f_type in pics:
        shutil.move(str(new_file), str(curr_folder / "Pictures"))
    elif f_type in vids:
        shutil.move(str(new_file), str(curr_folder / "Videos"))
    elif f_type in data or zipfile.is_zipfile(new_file):
        shutil.move(str(new_file), str(curr_folder / "Data"))
    else:
        text = ""

        if new_file.suffix.lower() == ".pdf":

            try:
                reader = PdfReader(new_file)
            except:
                continue
            
            for page in reader.pages:
                page_text = page.extract_text()
                
                if page_text:
                    text += page_text + " "

            words = text.split()
            first100 = " ".join(words[:75])

            folder = get_folder(first100, new_file.name)

            shutil.move(str(new_file), str(curr_folder / folder))
        
        elif new_file.suffix.lower() == ".docx":

            doc = Document(new_file)
            
            for para in doc.paragraphs:
                text += para.text + " "
            
            words = text.split()
            first100 = " ".join(words[:75])

            folder = get_folder(first100, new_file.name)

            shutil.move(str(new_file), str(curr_folder / folder))
