(* Queries *)

type milestone = {title: string; description: string option}

type project_column = {id: string; databaseId: int option}

module PullRequest_Milestone_and_Cards =
[%graphql
{|
  query backportInfo($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner,name: $repo) {
      pullRequest(number: $number) {
        milestone @bsRecord {
          title
          description
        }
        projectCards(first:100) {
          nodes {
            id
            column @bsRecord {
              id
              databaseId
            }
            project {
              columns(first:100) {
                nodes @bsRecord {
                  id
                  databaseId
                }
              }
            }
          }
        }
      }
    }
  }
|}]

module PullRequest_ID_and_Milestone =
[%graphql
{|
  query prInfo($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner,name: $repo) {
      pullRequest(number: $number) {
        id
        databaseId
        milestone {
          title
          description
        }
      }
    }
  }
|}]

module TeamMembership =
[%graphql
{|
  query teamMember($org: String!, $team: String!, $user: String!) {
    organization(login:$org) {
      team(slug:$team) {
        members(query:$user, first:1) {
          nodes { login }
        }
      }
    }
  }
|}]

module PullRequest_Refs =
[%graphql
{|
  query prRefs($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner, name:$repo) {
      pullRequest(number: $number) {
        id
        baseRefName
        baseRefOid @bsDecoder(fn: "Yojson.Basic.to_string")
        headRefName
        headRefOid @bsDecoder(fn: "Yojson.Basic.to_string")
        merged
      }
    }
  }
|}]

module Issue_Milestone =
[%graphql
{|
  query issueMilestone($owner: String!, $repo: String!, $number: Int!) {
    repository(owner:$owner, name:$repo) {
      issue(number:$number) {
        id
        milestone { id }
        timelineItems(itemTypes:[CLOSED_EVENT],last:1) {
          nodes {
            ... on ClosedEvent {
              closer {
                ... on PullRequest {
                  id
                  milestone { id }
                }
                ... on Commit {
                  associatedPullRequests(first: 2) {
                    nodes {
                      id
                      milestone { id }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
|}]

(* Mutations *)

module MoveCardToColumn =
[%graphql
{|
  mutation moveCard($card_id:ID!,$column_id:ID!) {
    moveProjectCard(input:{cardId:$card_id,columnId:$column_id}) {
      clientMutationId
    }
  }
|}]

module PostComment =
[%graphql
{|
  mutation addComment($id:ID!,$message:String!) {
    addComment(input:{subjectId:$id,body:$message}) {
      clientMutationId
    }
  }
|}]

module UpdateMilestone =
[%graphql
{|
  mutation updateMilestone($issue: ID!, $milestone: ID!) {
    updateIssue(input: {id: $issue, milestoneId: $milestone}) {
      clientMutationId
    }
  }
|}]
