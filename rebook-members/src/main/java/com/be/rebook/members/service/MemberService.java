package com.be.rebook.members.service;

import com.be.rebook.members.dto.UpdateDTO;
import com.be.rebook.members.dto.UserinfoDTO;
import com.be.rebook.members.entity.Majors;
import com.be.rebook.members.entity.Members;
import com.be.rebook.members.entity.Universities;
import com.be.rebook.members.repository.MajorsRepository;
import com.be.rebook.members.repository.MembersRepository;
import com.be.rebook.members.repository.UniversitiesRepository;
import com.be.rebook.common.exception.BaseException;
import com.be.rebook.common.exception.ErrorCode;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


// TODO: 시큐리티 로직 auth 서버에 의존하도록 리팩토링
@Service
@Transactional(readOnly = true)
public class MemberService {

    private final MembersRepository membersRepository;
    private final UniversitiesRepository universitiesRepository;
    private final MajorsRepository majorsRepository;

    public MemberService(MembersRepository membersRepository,
                         UniversitiesRepository universitiesRepository,
                         MajorsRepository majorsRepository){
        this.membersRepository = membersRepository;
        this.universitiesRepository = universitiesRepository;
        this.majorsRepository = majorsRepository;
    }

    @Transactional
    public Members updateUserNickname(String username, String nicknameToUpdate) {
        Members member = membersRepository.findByUsername(username)
                .orElseThrow(()->new BaseException(ErrorCode.NO_USER_INFO));

        Members updatedMember = member
                .toBuilder()
                .nickname(nicknameToUpdate)
                .build();

        membersRepository.save(updatedMember);
        return updatedMember;
    }

    @Transactional
    public Members updateUserUniversity(String username, String universityToUpdate) {
        Members member = membersRepository.findByUsername(username)
                .orElseThrow(()->new BaseException(ErrorCode.NO_USER_INFO));

        Long unvId = -1L;
        if (universityToUpdate != null) {
            unvId = universitiesRepository
                    .findByUniversity(universityToUpdate)
                    .orElseThrow(()->new BaseException(ErrorCode.NO_UNIVERSITY_INFO))
                    .getUnvId();
        }

        Members updatedMember = member
                .toBuilder()
                .university(unvId)
                .build();

        membersRepository.save(updatedMember);
        return updatedMember;
    }

    @Transactional
    public Members updateUserMajors(String username, String majorsToUpdate) {
        Members member = membersRepository.findByUsername(username)
                .orElseThrow(()->new BaseException(ErrorCode.NO_USER_INFO));

        String majors = null;

        if (majorsToUpdate != null){
            String[] majorList = majorsToUpdate.split(",");
            StringBuilder sb = new StringBuilder();
            for(String major : majorList){
                sb.append(majorsRepository
                        .findByMajor(major)
                        .orElseThrow(()->new BaseException(ErrorCode.NO_MAJOR_INFO))
                        .getMajorId());
                sb.append(",");
            }
            majors = sb.toString();
        }

        Members updatedMember = member
                .toBuilder()
                .majors(majors)
                .build();

        membersRepository.save(updatedMember);
        return updatedMember;
    }

    public Members deleteUser(String username) {
        Members member = membersRepository
                .findByUsername(username)
                .orElseThrow(()->new BaseException(ErrorCode.NO_USER_INFO));
        // List<RefreshTokens> refreshTokens = refreshTokensRepository.findByUsername(username);
        // for(RefreshTokens tokenToDelete : refreshTokens){
        //     refreshTokensRepository.delete(tokenToDelete);
        // }
        membersRepository.delete(member);
        return member;
    }

    public List<String> getUniversitiesList(String unvToSearch){
        List<Universities> universities = universitiesRepository
                .searchByUniversity(unvToSearch);
        if(universities.isEmpty()){
            throw new BaseException(ErrorCode.NO_UNIVERSITY_INFO);
        }

        List<String> returnList = new ArrayList<>();
        for(Universities unv : universities){
            returnList.add(unv.getUniversity());
        }
        return returnList;
    }

    public List<String> getMajorsList(String majorToSearch){
        List<Majors> majors = majorsRepository
                .searchByMajor(majorToSearch);

        if(majors.isEmpty()){
            throw new BaseException(ErrorCode.NO_MAJOR_INFO);
        }

        List<String> returnList = new ArrayList<>();
        for(Majors major : majors){
            returnList.add(major.getMajor());
        }
        return returnList;
    }

     public UserinfoDTO getUserinfo(String username){
         Members foundMember = membersRepository
                 .findByUsername(username)
                 .orElseThrow(()->new BaseException(ErrorCode.NO_USER_INFO));

         String returnNickname = "닉네임을 설정하세요.";
         if(foundMember.getNickname() != null){
             returnNickname = foundMember.getNickname();
         }

         String returnUnv = "대학교를 설정하세요.";
         if(foundMember.getUniversity() != null && foundMember.getUniversity() != -1L){
             Universities foundUnv = universitiesRepository
                     .findByUnvId(foundMember.getUniversity())
                     .orElseThrow(()->new BaseException(ErrorCode.NO_UNIVERSITY_INFO));
             returnUnv = foundUnv.getUniversity();
         }

         String returnMajors = "관심 전공을 설정하세요.";
         StringBuilder majorList = new StringBuilder();
         if(foundMember.getMajors()!= null){
             List<String> majorIdList = new ArrayList<>(Arrays.asList(foundMember.getMajors().split(",")));
             for(String id : majorIdList){
                 majorList.append(majorsRepository
                         .findByMajorId(id)
                         .orElseThrow(()->new BaseException(ErrorCode.NO_MAJOR_INFO))
                         .getMajor());
                 majorList.append(", ");
             }
             if (!majorList.isEmpty()) {
                 majorList.setLength(majorList.length() - 2);
             }
             returnMajors = majorList.toString();
         }

         return UserinfoDTO
                 .builder()
                 .username(foundMember.getUsername())
                 .nickname(returnNickname)
                 .university(returnUnv)
                 .majors(returnMajors)
                 .build();
     }
}

