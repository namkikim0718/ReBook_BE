package com.be.rebook.members.controller;

import com.be.rebook.members.dto.*;
import com.be.rebook.members.entity.Members;
import com.be.rebook.members.service.MemberService;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Pattern;

import org.springframework.http.ResponseEntity;
import com.be.rebook.common.argumentresolver.auth.Auth;
import com.be.rebook.common.argumentresolver.auth.MemberLoginInfo;
import com.be.rebook.common.config.BaseResponse;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;

@RestController
@RequestMapping("/members")
public class MembersController {

    private final MemberService memberService;

    public MembersController(MemberService memberService) {
        this.memberService = memberService;
    }

    @GetMapping("/me")
    public ResponseEntity<BaseResponse<UserinfoDTO>> getMethodName(@Auth MemberLoginInfo memberLoginInfo) {
        UserinfoDTO userInfo = memberService.getUserinfo(memberLoginInfo.getUsername());
        return ResponseEntity.ok().body(new BaseResponse<>(userInfo));
    }

    @PatchMapping("/nickname")
    public ResponseEntity<BaseResponse<Members>> updateUserNickname(@Auth MemberLoginInfo memberLoginInfo, UpdateNicknameDTO nicknameDTO){
        String username = memberLoginInfo.getUsername();
        String nicknameToUpdate = nicknameDTO.getNickname();
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.updateUserNickname(username, nicknameToUpdate)));
    }

    @PatchMapping("/university")
    public ResponseEntity<BaseResponse<Members>> updateUserUniversity(@Auth MemberLoginInfo memberLoginInfo, UpdateUniversityDTO universityDTO){
        String username = memberLoginInfo.getUsername();
        String universityToUpdate = universityDTO.getUniversity();
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.updateUserUniversity(username, universityToUpdate)));
    }

    @PatchMapping("/majors")
    public ResponseEntity<BaseResponse<Members>> updateUserMajors(@Auth MemberLoginInfo memberLoginInfo, UpdateMajorsDTO majorsDTO){
        String username = memberLoginInfo.getUsername();
        String majorsToUpdate = majorsDTO.getMajors();
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.updateUserMajors(username, majorsToUpdate)));
    }

    // 로그인 이후 마이페이지에서 바로 비밀번호 변경이라 이메일 인증 필요없음
    // todo : 패스워드 수정 로직 테스트하기
    @PatchMapping("/password")
    public ResponseEntity<BaseResponse<Members>> updateUserPassword(@Auth MemberLoginInfo memberLoginInfo, @Valid UpdatePasswordDTO passwordDTO){
        String username = memberLoginInfo.getUsername();
        String passwordToUpdate = passwordDTO.getPassword();
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.updateUserPassword(username, passwordToUpdate)));
    }
    //todo : 프로필 사진 변경 로직 테스트하기
    @PatchMapping("/profilePicture")
    public ResponseEntity<BaseResponse<Members>> updateUserPicture(@Auth MemberLoginInfo memberLoginInfo, MultipartFile picture){
        String username = memberLoginInfo.getUsername();
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.updateUserPicture(username, picture)));
    }

    //todo : 프로필 사진 삭제 로직 테스트하기
    @DeleteMapping("/profilePicture")
    public ResponseEntity<BaseResponse<Members>> deleteUserPicture(@Auth MemberLoginInfo memberLoginInfo){
        String username = memberLoginInfo.getUsername();
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.deleteUserPicture(username)));
    }

    @DeleteMapping
    public ResponseEntity<BaseResponse<Members>> deleteUser(@Auth MemberLoginInfo memberLoginInfo) {
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.deleteUser(memberLoginInfo.getUsername())));
    }

    @GetMapping("/universities")
    public ResponseEntity<BaseResponse<List<String>>> searchUniversities(
            @RequestParam("unvToSearch") String unvToSearch) {
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getUniversitiesList(unvToSearch)));
    }

    @GetMapping("/majors")
    public ResponseEntity<BaseResponse<List<String>>> searchMajors(
            @Pattern(regexp = ".*[^가-힣\\sA-Z()].*", message = "검색어 입력이 잘못 되었습니다.") @RequestParam("majorToSearch") String majorToSearch) {
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getMajorsList(majorToSearch)));
    }
}
