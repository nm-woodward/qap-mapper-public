import React from 'react';
import './ScoringData.css';

function ScoringData({ data }) {
  if (!data) {
    console.log('Scoring...');
    return <div className='scoring-data-block'>
            <h1>Scoring...</h1>
            </div>;
  }

  const { scored_activity_table, desirable_activity_total_points, scored_community_transportation, scored_previous_projects, total_points } = data;

  return (
    <div className='scoring-data-block'>
    <h1>{total_points} points</h1>
    <p>...across 3 geographic QAP categories. See <a href="https://receptive-muenster-7fe.notion.site/QAP-Mapper-2d274826428549879f6b29a4b7a82c9c?pvs=25" 
    target="_blank">project explainer page</a> for details.</p> 
    <h2>
      Desirable Activities: <nbsp></nbsp>
      {desirable_activity_total_points > 20 ? (
        <span> Maximum Points: ({desirable_activity_total_points} / 20)</span>
      ) : (
        `${desirable_activity_total_points} points`
      )}
    </h2>
      <table>
        <thead>
          <tr>
            <th>Desirable Activity</th>
            <th>Name</th>
            <th>Miles Away</th>
            <th>Points</th>
          </tr>
        </thead>
        <tbody>
          {scored_activity_table.map((activity, index) => {
            let rowClass = '';
            if (!activity.name) {
              rowClass = 'highlight-missing';
            } else if (activity.miles_away > 1) {
              rowClass = 'highlight-far';
            } else if (activity.miles_away > 0.5) {
              rowClass = 'highlight-closer';
            } else {
              rowClass = 'highlight-closest';
            }

            return (
              <tr key={index} className={rowClass}>
                <td>{activity.qapDesirableActivity}</td>
                <td>{activity.name ? activity.name : 'none within 1.5 miles'}</td>
                <td>{activity.miles_away}</td>
                <td>{activity.points}</td>
              </tr>
            );
          })}
        </tbody>
      </table>

      <h2>Community Transportation Options: {scored_community_transportation[0][0].community_transportation_total_points} points</h2>
      <h3>(A) Transit-Oriented Development: {scored_community_transportation[0][0].community_transportation_a_points} points</h3>
      {scored_community_transportation[1][0].name && 
      <p>{scored_community_transportation[1][0].name}: {scored_community_transportation[1][0].miles_away} miles away (walking)</p>
      }
      <h3>(B) Access to Public Transportation: {scored_community_transportation[0][0].community_transportation_b_points} points</h3>
      {scored_community_transportation[2][0].name && 
      <p>{scored_community_transportation[2][0].name}: {scored_community_transportation[2][0].miles_away} miles away (walking)</p>
      }
      {scored_previous_projects.map((project, index) => (
        <div key={index}>
          <h2>Previous Projects: {project.previous_project_total_points} points</h2>
          <h3>(A) Awards in Jurisdiction: {project.previous_project_a_points} points</h3>
           {project.jurisdiction_name ? <p> Last project in {project.jurisdiction_name}: {project.most_recent_year} </p> : <p>Unincorporated jurisdiction</p> }
          <h3>(B) Awards within a Mile: {project.previous_project_b_points} points</h3>
          {project.previous_project_b_points > 0 && 
            <p>Last award within a mile: {project.b_no_awards_1mi_since} years ago</p>
          }
          {project.previous_project_b_points === 0 && 
          <p>Last award within a mile: within last 2 years</p>}
          {project.previous_project_b_points > 0 && project.b_no_awards_1mi_since <= 3 &&
          <p>Transit hub within 1 mile walk</p>}
        </div>
      ))}
    </div>
  );
}

export default ScoringData;

