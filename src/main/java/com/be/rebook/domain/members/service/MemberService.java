package com.be.rebook.domain.members.service;

import com.be.rebook.domain.members.dto.UpdateDTO;
import com.be.rebook.domain.members.dto.UserinfoDTO;
import com.be.rebook.domain.members.entity.Majors;
import com.be.rebook.domain.members.entity.Members;
import com.be.rebook.domain.members.entity.RefreshTokens;
import com.be.rebook.domain.members.entity.Universities;
import com.be.rebook.domain.members.jwt.JWTUtil;
import com.be.rebook.domain.members.repository.MajorsRepository;
import com.be.rebook.domain.members.repository.MembersRepository;
import com.be.rebook.domain.members.repository.RefreshTokensRepository;
import com.be.rebook.domain.members.repository.UniversitiesRepository;
import com.be.rebook.global.exception.BaseException;
import com.be.rebook.global.exception.ErrorCode;
import io.jsonwebtoken.ExpiredJwtException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Service
public class MemberService {

    private final MembersRepository membersRepository;
    private final UniversitiesRepository universitiesRepository;
    private final MajorsRepository majorsRepository;
    private final JWTUtil jwtUtil;

    private static final Logger memberServiceLogger = LoggerFactory.getLogger(MemberService.class);
    private final RefreshTokensRepository refreshTokensRepository;

    public MemberService(MembersRepository membersRepository,
                         JWTUtil jwtUtil,
                         RefreshTokensRepository refreshTokensRepository,
                         UniversitiesRepository universitiesRepository,
                         MajorsRepository majorsRepository){
        this.membersRepository = membersRepository;
        this.jwtUtil = jwtUtil;
        this.refreshTokensRepository = refreshTokensRepository;
        this.universitiesRepository = universitiesRepository;
        this.majorsRepository = majorsRepository;
    }

    //todo : 특문 걸러내는거 inputVerifier에 통합할건지?
    private Boolean checkSpecialCharacters(String input){
        return input.matches(".*[^a-zA-Z0-9\\uAC00-\\uD7AF].*");
    }

    @Transactional
    public Members updateUser(String token, UpdateDTO membersUpdateDTO) {
        try {
            jwtUtil.isExpired(token);
        } catch (ExpiredJwtException e) {
            //EXPIRED_TOKEN
            memberServiceLogger.error("회원 정보 업데이트 오류 : 토큰 만료됨 {}", ErrorCode.EXPIRED_TOKEN);
            throw new BaseException(ErrorCode.EXPIRED_TOKEN);
        }

        String username = jwtUtil.getUsername(token);
        Boolean isUsernameExists = membersRepository.existsByUsername(username);

        if (Boolean.FALSE.equals(isUsernameExists)){
            //NO_USER_INFO
            memberServiceLogger.error("회원 정보 업데이트 오류 : 해당 유저 없음, 코드 {}", ErrorCode.NO_USER_INFO);
            throw new BaseException(ErrorCode.NO_USER_INFO);
        }

        Members member = membersRepository.findByUsername(username);
        String nickname = null;
        Long unvId = -1L;
        String majors = null;

        String nicknameToUpdate = membersUpdateDTO.getNickname();
        String unvToUpdate = membersUpdateDTO.getUniversity();

        if ((nicknameToUpdate != null && checkSpecialCharacters(nicknameToUpdate)) ||
                (unvToUpdate != null && checkSpecialCharacters(unvToUpdate))) {
            //BAD_INPUT
            memberServiceLogger.error("회원 정보 업데이트 오류 : 입력 형식 잘못됨, 코드 {}", ErrorCode.BAD_INPUT);
            throw new BaseException(ErrorCode.BAD_INPUT);
        }

        if (nicknameToUpdate != null) {
            nickname = membersUpdateDTO.getNickname();
        }

        // string으로 들어온 학교를 universities 테이블에서 조회해서 id값 얻어오고
        // 그 아이디값 저장
        if (unvToUpdate != null) {
            unvId = universitiesRepository.findByUniversity(unvToUpdate).getUnvId();
        }

        // , 콤마로 전공명,전공명,전공명 이런식으로 들어온 데이터 ,로 나눠서 각 전공명별로 아이디값 조회해서
        // 멤버 majors 항목에 1,2,3,4,5 식의 스트링으로 저장하기
        String majorsToUpdate = membersUpdateDTO.getMajors();
        if (majorsToUpdate != null){
            if(majorsToUpdate.matches(".*[^a-zA-Z0-9,\\uAC00-\\uD7AF].*")){
                //BAD_INPUT
                memberServiceLogger.error("회원 정보 업데이트 오류 : 입력 형식 잘못됨, 코드 {}", ErrorCode.BAD_INPUT);
                throw new BaseException(ErrorCode.BAD_INPUT);
            }

            String[] majorList = membersUpdateDTO.getMajors().split(",");
            StringBuilder sb = new StringBuilder();
            for(String major : majorList){
                sb.append(majorsRepository.findByMajor(major).getMajorId());
                sb.append(",");
            }
            majors = sb.toString();
        }

        Members updatedMember = member
                .toBuilder()
                .nickname(nickname)
                .university(unvId)
                .majors(majors)
                .build();

        membersRepository.save(updatedMember);
        return updatedMember;
    }

    public Members deleteUser(String token) {
        try {
            jwtUtil.isExpired(token);
        } catch (ExpiredJwtException e) {
            //EXPIRED_TOKEN
            memberServiceLogger.error("회원 탈퇴 오류 : 토큰 만료됨, 코드: {}", ErrorCode.EXPIRED_TOKEN);
            throw new BaseException(ErrorCode.EXPIRED_TOKEN);
        }

        String username = jwtUtil.getUsername(token);
        Boolean isUsernameExists = membersRepository.existsByUsername(username);

        if(Boolean.FALSE.equals(isUsernameExists)){
            //NO_USER_INFO
            memberServiceLogger.error("회원 탈퇴 오류 : 유저 없음, 코드: {}", ErrorCode.NO_USER_INFO);
            throw new BaseException(ErrorCode.NO_USER_INFO);
        }

        Members member = membersRepository.findByUsername(username);
        List<RefreshTokens> refreshTokens = refreshTokensRepository.findByUsername(username);
        for(RefreshTokens tokenToDelete : refreshTokens){
            refreshTokensRepository.delete(tokenToDelete);
        }
        membersRepository.delete(member);
        return member;
    }

    public List<String> getUniversitiesList(String unvToSearch){
        if(unvToSearch.matches(".*[^가-힣\\sA-Z()].*")){
            //BAD_INPUT
            memberServiceLogger.error("검색어로 대학 목록 불러오기 오류 : 입력 형식 잘못됨, 코드: {}", ErrorCode.BAD_INPUT);
            throw new BaseException(ErrorCode.BAD_INPUT);
        }
        List<Universities> universitiesList = universitiesRepository.searchByUniversity(unvToSearch);
        List<String> returnList = new ArrayList<>();
        for(Universities unv : universitiesList){
            returnList.add(unv.getUniversity());
        }
        return returnList;
    }

    public List<String> getMajorsList(String majorToSearch){
        if(majorToSearch.matches(".*[^가-힣\\sA-Z()].*")){
            //BAD_INPUT
            memberServiceLogger.error("검색어로 전공 목록 불러오기 오류 : 입력 형식 잘못됨, 코드: {}", ErrorCode.BAD_INPUT);
            throw new BaseException(ErrorCode.BAD_INPUT);
        }
        List<Majors> majorsList = majorsRepository.searchByMajor(majorToSearch);
        List<String> returnList = new ArrayList<>();
        for(Majors major : majorsList){
            returnList.add(major.getMajor());
        }
        return returnList;
    }

    public UserinfoDTO getUserinfo(String token){
        try {
            jwtUtil.isExpired(token);
        } catch (ExpiredJwtException e) {
            //EXPIRED_TOKEN
            memberServiceLogger.error("회원 정보 조회 오류 : 토큰 만료됨, 코드: {}", ErrorCode.EXPIRED_TOKEN);
            throw new BaseException(ErrorCode.EXPIRED_TOKEN);
        }

        String username = jwtUtil.getUsername(token);
        Boolean isUsernameExists = membersRepository.existsByUsername(username);

        if(Boolean.FALSE.equals(isUsernameExists)){
            //NO_USER_INFO
            memberServiceLogger.error("회원 정보 조회 오류 : 유저 없음, 코드: {}", ErrorCode.NO_USER_INFO);
            throw new BaseException(ErrorCode.NO_USER_INFO);
        }
        Members foundMember = membersRepository.findByUsername(username);

        List<String> majorIdList = new ArrayList<>(Arrays.asList(foundMember.getMajors().split(",")));
        StringBuilder majorList = new StringBuilder();
        for(String id : majorIdList){
            majorList.append(majorsRepository.findByMajorId(id).getMajor());
            majorList.append(", ");
        }
        if (!majorList.isEmpty()) {
            majorList.setLength(majorList.length() - 2);
        }

        return UserinfoDTO
                .builder()
                .username(foundMember.getUsername())
                .nickname(foundMember.getNickname())
                .university(universitiesRepository.findByUnvId(foundMember.getUniversity()).getUniversity())
                .majors(majorList.toString())
                .build();
    }
}

