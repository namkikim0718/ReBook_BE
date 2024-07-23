package com.be.rebook.domain.members.service;

import com.be.rebook.domain.members.dto.UpdateDTO;
import com.be.rebook.domain.members.entity.Members;
import com.be.rebook.domain.members.entity.RefreshTokens;
import com.be.rebook.domain.members.jwt.JWTUtil;
import com.be.rebook.domain.members.repository.MajorsRepository;
import com.be.rebook.domain.members.repository.MembersRepository;
import com.be.rebook.domain.members.repository.RefreshTokensRepository;
import com.be.rebook.domain.members.repository.UniversitiesRepository;
import io.jsonwebtoken.ExpiredJwtException;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class MemberService {

    private MembersRepository membersRepository;
    private UniversitiesRepository universitiesRepository;
    private MajorsRepository majorsRepository;
    private JWTUtil jwtUtil;

    private RefreshTokensRepository refreshTokensRepository;

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

    public Members getMemberByUsername(String username) {
        return membersRepository.findByUsername(username);
    }

    public ResponseEntity<Members> updateUser(String token, UpdateDTO membersUpdateDTO) {
        try {
            jwtUtil.isExpired(token);
        } catch (ExpiredJwtException e) {
            System.out.println("회원 정보 업데이트 오류 : 토큰 만료됨");
            return ResponseEntity.status(HttpServletResponse.SC_BAD_REQUEST).build();
        }

        String username = jwtUtil.getUsername(token);

        if (membersRepository.existsByUsername(username)) {
            Members member = membersRepository.findByUsername(username);

            if (membersUpdateDTO.getNickname() != null) {
                member.setNickname(membersUpdateDTO.getNickname());
            }

            // string으로 들어온 학교를 universities 테이블에서 조회해서 id값 얻어오고
            // 그 아이디값 저장
            if (membersUpdateDTO.getUniversity() != null) {
                String school = membersUpdateDTO.getUniversity();
                member.setUniversity(universitiesRepository.findByUniversity(school).getUnvId());
            }

            // , 콤마로 전공명,전공명,전공명 이런식으로 들어온 데이터 ,로 나눠서 각 전공명별로 아이디값 조회해서
            // 멤버 majors 항목에 1,2,3,4,5 식의 스트링으로 저장하기
            // -> 이상함 후에 어떻게 처리할지 생각해봐야됨 TODO
            if (membersUpdateDTO.getMajors() != null){
                String[] majorList = membersUpdateDTO.getMajors().split(",");
                StringBuilder sb = new StringBuilder();
                for(String major : majorList){
                    sb.append(majorsRepository.findByMajor(major).getMajorId());
                    sb.append(",");
                }
                member.setMajors(sb.toString());
            }

            Members updatedMember = membersRepository.save(member);
            return ResponseEntity.ok(updatedMember);
        } else {
            System.out.println("회원 정보 업데이트 오류 : 해당 유저 없음");
            return ResponseEntity.notFound().build();
        }
    }

    // TroubleShooting
    //403 오류 해결
    // -> SecurityContextHolder에 토큰 생성할때 등록
    // -> isExpired에서 현재 날짜로 그 전에 토큰의 유효기간이 끝나는게 아니라 현재 날짜 + 유효기간으로 판단.
    public ResponseEntity<Void> deleteUser(String token) {
        try {
            jwtUtil.isExpired(token);
        } catch (ExpiredJwtException e) {
            System.out.println("회원 탈퇴 오류 : 토큰 만료됨");
            return ResponseEntity.status(HttpServletResponse.SC_BAD_REQUEST).build();
        }

        String username = jwtUtil.getUsername(token);

        if (membersRepository.existsByUsername(username)) {
            Members member = membersRepository.findByUsername(username);
            List<RefreshTokens> refreshTokens = refreshTokensRepository.findByUsername(username);
            for(RefreshTokens tokenToDelete : refreshTokens){
                refreshTokensRepository.delete(tokenToDelete);
            }
            membersRepository.delete(member);
            return ResponseEntity.ok().build();
        } else {
            System.out.println("회원 탈퇴 오류 : 유저 없음 ");
            return ResponseEntity.notFound().build();
        }
    }
}

