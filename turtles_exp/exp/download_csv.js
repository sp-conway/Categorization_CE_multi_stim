// from Winson Yang youtube tutorial on jsPsych

function download_csv(csv, file_name){
  var csv_file;
  var download_link;

  // retrieve csv file from experiment
  // Retrieve csv file from experiment
  csv_file = new Blob([csv], {type: "text/csv"});

  // Download link
  download_link = document.createElement("a");

  // Retrieve File name
  download_link.download = file_name;

  // Create a link to the file
  download_link.href = window.URL.createObjectURL(csv_file);

  // Hide download link
  download_link.style.display = 'none';

  // Add link to the DOM
  document.body.appendChild(download_link);

  download_link.click();
}
